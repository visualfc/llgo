"""Compare the stock Go runtime with a larger Windows exception stack reserve."""

import json
import os
import re
import runpy
import subprocess
import time
from pathlib import Path


def main():
    root = Path.cwd()
    output = root / "diag-results"
    helper = runpy.run_path(str(Path(__file__).with_name("diagnose_mingw_go.py")))
    context = helper["record_exception_context"](output)
    large_context = context["context_bytes"] > 8192
    results = []

    def run(label, command, timeout=180, cwd=root):
        log = output / f"{label}.log"
        print(f"START {label}: {command}", flush=True)
        started = time.monotonic()
        with log.open("wb") as stream:
            process = subprocess.Popen(command, cwd=cwd, stdout=stream, stderr=subprocess.STDOUT)
            try:
                status = process.wait(timeout=timeout)
            except subprocess.TimeoutExpired:
                subprocess.run(["taskkill", "/PID", str(process.pid), "/T", "/F"],
                               stdout=stream, stderr=subprocess.STDOUT, check=False)
                process.wait()
                status = "timeout"
        result = {"label": label, "returncode": status,
                  "seconds": round(time.monotonic() - started, 3), "log": log.name}
        results.append(result)
        (output / "results.json").write_text(json.dumps(results, indent=2))
        print(json.dumps(result), flush=True)
        return result, log.read_bytes()

    flags = ["-ldflags=-linkmode=external -extldflags=-lsynchronization", "-covermode=atomic"]
    reproduced = False
    for iteration in range(1, (30 if large_context else 5) + 1):
        label = f"stock-{iteration:03d}"
        result, raw = run(label, ["go", "test", *flags, "-count=1", "-timeout=45m",
                                  "-coverprofile=" + str(output / f"coverage-{label}.txt"), "./test/go"])
        text = raw.decode("utf-8", errors="replace")
        if result["returncode"] != 0:
            print(text, flush=True)
            if ("exit status 0xc0000005" in text and
                    re.search(r"(?m)^FAIL\s+github\.com/xgo-dev/llgo/test/go\s", text) and
                    "--- FAIL:" not in text):
                reproduced = True
                break
            raise SystemExit("Stock control had a different failure; inspect its log.")

    # Keep the installed SDK and all repository tests unchanged. The overlay
    # rebuilds the standard runtime only for the commands that explicitly use it.
    goroot = Path(subprocess.check_output(["go", "env", "GOROOT"], text=True).strip())
    source = goroot / "src/runtime/stack.go"
    original = source.read_text(encoding="utf-8")
    before, after = "goos.IsWindows*4096", "goos.IsWindows*16384"
    if original.count(before) != 1:
        raise SystemExit("Unexpected Go stackSystem source; refusing an ambiguous overlay.")
    patched_source = output / "runtime-stack-16384.go"
    patched_source.write_text(original.replace(before, after), encoding="utf-8")
    overlay = output / "runtime-overlay.json"
    overlay.write_text(json.dumps({"Replace": {str(source): str(patched_source)}}, indent=2))
    patched_flags = ["-overlay=" + str(overlay), *flags]
    executable = output / "patched.test.exe"
    built, raw = run("build-patched", ["go", "test", "-c", "-cover", *patched_flags,
                                       "-o", str(executable), "./test/go"], timeout=300)
    if built["returncode"] != 0:
        print(raw.decode("utf-8", errors="replace"), flush=True)
        raise SystemExit("Patched test build failed.")

    repetitions = 100 if large_context else 5
    for iteration in range(1, repetitions + 1):
        label = f"patched-{iteration:03d}"
        result, raw = run(label, ["go", "test", *patched_flags, "-count=1", "-timeout=45m",
                                  "-coverprofile=" + str(output / f"coverage-{label}.txt"), "./test/go"])
        text = raw.decode("utf-8", errors="replace")
        if result["returncode"] != 0 or not re.search(r"(?m)^ok\s+github\.com/xgo-dev/llgo/test/go\s", text):
            print(text, flush=True)
            raise SystemExit("The patched runtime still fails the complete test package.")

    count = 200 if large_context else 20
    dumps = output / "patched-dumps"
    dumps.mkdir(exist_ok=True)
    captured, raw = run("patched-stress", [os.environ["LLGO_DIAG_PROCDUMP"],
        "-accepteula", "-ma", "-e", "-t", "-x", str(dumps), str(executable),
        "-test.v", f"-test.count={count}", "-test.timeout=5m"], timeout=330, cwd=root / "test/go")
    text, target_status = helper["captured_process_result"](raw)
    (output / "patched-stress.decoded.log").write_text(text, encoding="utf-8")
    if target_status != 0 or "PASS" not in text.splitlines():
        print("\n".join(text.splitlines()[-160:]), flush=True)
        raise SystemExit("The patched runtime failed under native crash capture.")
    for dump in dumps.glob("*.dmp"):
        dump.unlink()

    summary = {"large_context": large_context, "context": context,
               "stock_reproduced_access_violation": reproduced,
               "patched_go_test_passes": repetitions,
               "patched_same_process_passes": count,
               "patched_stress_test_starts": sum(line.startswith("=== RUN") for line in text.splitlines()),
               "classification": "matched-control-fixed" if reproduced else "control-not-reproduced"}
    (output / "comparison.json").write_text(json.dumps(summary, indent=2))
    print("COMPARISON " + json.dumps(summary), flush=True)
    with open(os.environ["GITHUB_STEP_SUMMARY"], "a", encoding="utf-8") as stream:
        stream.write("```json\n" + json.dumps(summary, indent=2) + "\n```\n")


if __name__ == "__main__":
    main()
