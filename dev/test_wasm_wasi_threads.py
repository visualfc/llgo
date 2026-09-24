#!/usr/bin/env python3

"""Exercise the experimental WASI pthread backend under WAMR."""

import os
import pathlib
import shutil
import subprocess
import tempfile


ROOT = pathlib.Path(__file__).resolve().parent.parent
LLGO = os.environ.get("LLGO", "llgo")
IWASM = os.environ.get("IWASM", "iwasm")


def run_probe(env, directory, name, fixture, tags, marker, timeout):
    module = pathlib.Path(directory) / f"{name}.wasm"
    subprocess.run(
        [LLGO, "build", "-target", "wasi", "-tags", tags, "-o", str(module),
         str(ROOT / "internal/build/testdata" / fixture)],
        check=True,
        env=env,
        timeout=180,
    )
    result = subprocess.run(
        [IWASM, "--max-threads=8", "--stack-size=1048576",
         "--heap-size=67108864", str(module)],
        capture_output=True,
        text=True,
        timeout=timeout,
    )
    print(result.stdout, end="")
    print(result.stderr, end="")
    if result.returncode != 0 or (
        marker not in result.stdout.splitlines()
        and marker not in result.stderr.splitlines()
    ):
        raise SystemExit(f"WAMR {name} probe failed with exit code {result.returncode}")


def main():
    if shutil.which(IWASM) is None:
        raise SystemExit(f"WAMR runner not found: {IWASM}")

    env = os.environ.copy()
    env["LLGO_ROOT"] = str(ROOT)
    env["LLGO_WASI_THREADS"] = "1"
    with tempfile.TemporaryDirectory(prefix="llgo-wasi-threads-") as directory:
        run_probe(env, directory, "threads", "wasm-wasi-threads", "nogc",
                  "wasi threads ok", 30)
        run_probe(env, directory, "threaded-gc", "wasm-wasi-threaded-gc",
                  "llgo.wasm.gc.linear", "wasi threaded gc ok", 120)


if __name__ == "__main__":
    main()
