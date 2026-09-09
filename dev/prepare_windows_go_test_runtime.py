"""Prepare a temporary host-test workaround for golang/go#81238.

Only the explicit Go test command using the returned overlay is affected.
Remove this workaround when the pinned Go release fixes its Windows reserve.
"""

import ctypes
import json
import os
import platform
import subprocess
from pathlib import Path


def exception_context_bytes():
    kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
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
    # Windows/amd64 CONTEXT_ALL | CONTEXT_XSTATE. The size includes XSTATE
    # enabled by the host, even when this process never uses AMX instructions.
    ok = initialize(None, 0x0010005F, ctypes.byref(context), ctypes.byref(required))
    error = ctypes.get_last_error()
    if ok or error != 122 or required.value == 0:
        raise RuntimeError(f"InitializeContext size query returned {ok}, error {error}")
    return required.value


def prepare(output):
    env = json.loads(subprocess.check_output(
        ["go", "env", "-json", "GOOS", "GOARCH", "GOROOT", "GOVERSION"], text=True))
    result = {"go_version": env["GOVERSION"], "overlay": ""}
    if (os.name != "nt" or env["GOOS"] != "windows" or env["GOARCH"] != "amd64"
            or platform.machine().lower() not in ("amd64", "x86_64")):
        result["reason"] = "not a native Windows/amd64 host test"
        return result

    size = exception_context_bytes()
    result["context_bytes"] = size
    # Leave room for EXCEPTION_RECORD, kernel alignment, and ntdll/Go handler
    # frames in addition to the context. See go.dev/cl/828724 for measurements.
    required = size + 2048
    if required <= 4096:
        result["reason"] = "the stock reserve covers this host"
        return result
    reserve = 16384
    if required > reserve:
        raise RuntimeError(f"Windows needs {required} bytes, exceeding the validated {reserve}-byte reserve")

    source = Path(env["GOROOT"]) / "src/runtime/stack.go"
    original = source.read_text(encoding="utf-8")
    before = "goos.IsWindows*4096"
    if original.count(before) != 1:
        raise RuntimeError("Go's stackSystem source changed; review or remove the temporary Windows workaround")
    output = Path(output).resolve()
    output.mkdir(parents=True, exist_ok=True)
    patched = output / "runtime-stack.go"
    patched.write_text(original.replace(before, f"goos.IsWindows*{reserve}"), encoding="utf-8")
    overlay = output / "runtime-overlay.json"
    overlay.write_text(json.dumps({"Replace": {str(source): str(patched)}}, indent=2), encoding="utf-8")
    result.update(overlay=str(overlay), reserve_bytes=reserve,
                  reason="enlarge the host Go test runtime's Windows exception reserve")
    (output / "configuration.json").write_text(json.dumps(result, indent=2), encoding="utf-8")
    return result


def main():
    result = prepare(Path(os.environ["RUNNER_TEMP"]) / "llgo-go-test-runtime")
    print(json.dumps(result, indent=2), flush=True)
    with open(os.environ["GITHUB_OUTPUT"], "a", encoding="utf-8") as stream:
        stream.write("overlay=" + result["overlay"] + "\n")
    with open(os.environ["GITHUB_STEP_SUMMARY"], "a", encoding="utf-8") as stream:
        stream.write("Windows host Go test runtime:\n\n```json\n")
        stream.write(json.dumps(result, indent=2) + "\n```\n")


if __name__ == "__main__":
    main()
