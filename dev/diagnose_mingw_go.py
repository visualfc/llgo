"""Reproduce the unmodified PR 2549 host test package on Windows runners."""

import ctypes
import json
import os
import subprocess
import time
from pathlib import Path


def record_exception_context(output_dir):
    kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
    features = kernel32.GetEnabledXStateFeatures
    features.argtypes = []
    features.restype = ctypes.c_uint64
    initialize = kernel32.InitializeContext
    initialize.argtypes = [
        ctypes.c_void_p,
        ctypes.c_uint32,
        ctypes.POINTER(ctypes.c_void_p),
        ctypes.POINTER(ctypes.c_uint32),
    ]
    initialize.restype = ctypes.c_int
    context = ctypes.c_void_p()
    required = ctypes.c_uint32()
    # CONTEXT_ALL | CONTEXT_XSTATE on Windows/amd64. A null buffer queries size.
    ok = initialize(None, 0x0010005F, ctypes.byref(context), ctypes.byref(required))
    error = ctypes.get_last_error()
    if ok or error != 122:  # ERROR_INSUFFICIENT_BUFFER is the expected result.
        raise RuntimeError(f"InitializeContext returned {ok}, error {error}")
    info = {
        "enabled_xstate_features": hex(features()),
        "context_bytes": required.value,
        "stock_go_windows_stack_reserve": 4096,
    }
    (output_dir / "exception-context.json").write_text(json.dumps(info, indent=2))
    print("WINDOWS EXCEPTION CONTEXT " + json.dumps(info), flush=True)


def main():
    output_dir = Path("diag-results").resolve()
    executable = output_dir / "go.test.exe"
    # Match the package directory used by `go test ./test/go`.
    package_dir = Path("test/go").resolve()
    record_exception_context(output_dir)
    results = []
    cases = [("same-process-200", 200, True)]
    cases += [(f"fresh-{iteration:03d}", 1, False) for iteration in range(1, 101)]
    for label, count, capture in cases:
        log = output_dir / f"{label}.log"
        command = [
            str(executable),
            "-test.v",
            f"-test.count={count}",
            "-test.timeout=5m" if capture else "-test.timeout=90s",
            "-test.coverprofile=" + str(output_dir / f"coverage-{label}.txt"),
        ]
        dump_dir = output_dir / "dumps"
        if capture:
            dump_dir.mkdir(exist_ok=True)
            # -e captures unhandled exceptions; -t also captures explicit
            # ExitProcess paths, which can otherwise leave only 0xc0000005.
            command = [
                os.environ["LLGO_DIAG_PROCDUMP"],
                "-accepteula", "-ma", "-e", "-t", "-x", str(dump_dir),
            ] + command
        print(f"START {label}: {command}", flush=True)
        started = time.monotonic()
        with log.open("wb") as stream:
            process = subprocess.Popen(
                command, cwd=package_dir, stdout=stream, stderr=subprocess.STDOUT
            )
            try:
                returncode = process.wait(timeout=330 if capture else 105)
            except subprocess.TimeoutExpired:
                subprocess.run(
                    ["taskkill", "/PID", str(process.pid), "/T", "/F"],
                    stdout=stream, stderr=subprocess.STDOUT, check=False,
                )
                process.wait()
                returncode = "timeout"
        text = log.read_text(encoding="utf-8", errors="replace")
        result = {
            "label": label,
            "test_count": count,
            "returncode": returncode,
            "seconds": round(time.monotonic() - started, 3),
            "log": log.name,
            "tests_started": sum(line.startswith("=== RUN") for line in text.splitlines()),
        }
        results.append(result)
        (output_dir / "results.json").write_text(json.dumps(results, indent=2))
        print(json.dumps(result), flush=True)
        if returncode != 0 or "PASS" not in text.splitlines():
            print("FAILURE LOG TAIL", flush=True)
            print("\n".join(text.splitlines()[-160:]), flush=True)
            raise SystemExit(1)
        if capture:
            # Keep crash dumps for failures; a passing process's termination
            # dump is unnecessary once its complete log has been preserved.
            for dump in dump_dir.glob("*.dmp"):
                dump.unlink()
    print("All 200 same-process repetitions and 100 fresh processes passed.", flush=True)


if __name__ == "__main__":
    main()
