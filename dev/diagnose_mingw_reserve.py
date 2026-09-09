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

    def run(label, command, timeout=180, cwd=root, env=None):
        log = output / f"{label}.log"
        print(f"START {label}: {command}", flush=True)
        started = time.monotonic()
        with log.open("wb") as stream:
            process = subprocess.Popen(command, cwd=cwd, env=env,
                                       stdout=stream, stderr=subprocess.STDOUT)
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

    # This control uses only Go's internal linker, with no MinGW C runtime.
    pure_env = dict(os.environ, CGO_ENABLED="0")
    pure_reproduced = False
    for iteration in range(1, 11):
        label = f"stock-pure-go-{iteration:03d}"
        result, raw = run(label, ["go", "test", "-count=1", "-timeout=45m",
            "-covermode=atomic", "-coverprofile=" + str(output / f"coverage-{label}.txt"),
            "./test/go"], env=pure_env)
        text = raw.decode("utf-8", errors="replace")
        if result["returncode"] != 0:
            print(text, flush=True)
            if ("exit status 0xc0000005" in text and
                    re.search(r"(?m)^FAIL\s+github\.com/xgo-dev/llgo/test/go\s", text) and
                    "--- FAIL:" not in text):
                pure_reproduced = True
                break
            raise SystemExit("The pure Go control had a different failure.")

    # Use the exact preparation code proposed for the normal Go workflow.
    prepare = runpy.run_path(str(Path(__file__).with_name("prepare_windows_go_test_runtime.py")))["prepare"]
    configuration = prepare(output / "go-test-runtime")
    print("RUNTIME CONFIGURATION " + json.dumps(configuration), flush=True)
    if configuration.get("reserve_bytes") != 16384:
        raise SystemExit("This comparison requires an affected host and the 16 KiB workaround.")
    patched_flags = ["-overlay=" + configuration["overlay"], *flags]
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

    for iteration in range(1, 11):
        label = f"patched-pure-go-{iteration:03d}"
        result, raw = run(label, ["go", "test", "-overlay=" + configuration["overlay"],
            "-count=1", "-timeout=45m", "-covermode=atomic",
            "-coverprofile=" + str(output / f"coverage-{label}.txt"), "./test/go"], env=pure_env)
        if result["returncode"] != 0:
            print(raw.decode("utf-8", errors="replace"), flush=True)
            raise SystemExit("The patched pure Go test command failed.")

    count = 200 if large_context else 20
    dumps = output / "patched-dumps"
    dumps.mkdir(exist_ok=True)
    captured, raw = run("patched-stress", [os.environ["LLGO_DIAG_PROCDUMP"],
        "-accepteula", "-ma", "-e", "-t", "-x", str(dumps), str(executable),
        "-test.v", f"-test.count={count}", "-test.timeout=5m"], timeout=330, cwd=root / "test/go")
    text, target_status = helper["captured_process_result"](raw)
    captured["collector_returncode"] = captured["returncode"]
    captured["returncode"] = target_status
    (output / "results.json").write_text(json.dumps(results, indent=2))
    (output / "patched-stress.decoded.log").write_text(text, encoding="utf-8")
    if target_status != 0 or "PASS" not in text.splitlines():
        print("\n".join(text.splitlines()[-160:]), flush=True)
        raise SystemExit("The patched runtime failed under native crash capture.")
    for dump in dumps.glob("*.dmp"):
        dump.unlink()

    summary = {"large_context": large_context, "context": context,
               "stock_reproduced_access_violation": reproduced,
               "stock_pure_go_reproduced_access_violation": pure_reproduced,
               "patched_go_test_passes": repetitions,
               "patched_pure_go_test_passes": 10,
               "patched_same_process_passes": count,
               "patched_stress_test_starts": sum(line.startswith("=== RUN") for line in text.splitlines()),
               "classification": "matched-control-fixed" if reproduced else "control-not-reproduced"}
    (output / "comparison.json").write_text(json.dumps(summary, indent=2))
    print("COMPARISON " + json.dumps(summary), flush=True)
    with open(os.environ["GITHUB_STEP_SUMMARY"], "a", encoding="utf-8") as stream:
        stream.write("```json\n" + json.dumps(summary, indent=2) + "\n```\n")


if __name__ == "__main__":
    main()
