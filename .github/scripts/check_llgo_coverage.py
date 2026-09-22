#!/usr/bin/env python3

"""Reject an LLGo CI profile that Codecov would accept but cannot prove useful."""

from pathlib import Path
import sys


MODULE = "github.com/xgo-dev/llgo/"
REQUIRED_HIT_FILES = {
    "test/coverageprobe/probe_llgo.go",
    "test/coverageprobe/testdata/dependency/dependency_llgo.go",
}


def source_path(position: str) -> str:
    filename, separator, _ = position.rpartition(":")
    if not separator:
        raise ValueError(f"invalid coverage position: {position!r}")
    filename = filename.replace("\\", "/")
    if filename.startswith(MODULE):
        filename = filename[len(MODULE) :]
    return filename


def main() -> int:
    if len(sys.argv) != 2:
        print(f"usage: {Path(sys.argv[0]).name} COVERPROFILE", file=sys.stderr)
        return 2

    lines = Path(sys.argv[1]).read_text(encoding="utf-8").splitlines()
    if not lines or lines[0] != "mode: atomic":
        raise ValueError("LLGo CI coverage must use atomic mode")

    hits = set()
    misses = set()
    files = set()
    for line in lines[1:]:
        position, statements, count = line.rsplit(" ", 2)
        int(statements)
        path = source_path(position)
        files.add(path)
        if int(count) > 0:
            hits.add(path)
        elif int(statements) > 0:
            misses.add(path)

    missing = sorted(REQUIRED_HIT_FILES - hits)
    if missing:
        raise ValueError("LLGo coverage is missing executed probe files: " + ", ".join(missing))
    if REQUIRED_HIT_FILES - misses:
        raise ValueError("LLGo coverage lost the probes' deliberately unexecuted statements")
    test_files = sorted(path for path in files if path.endswith("_test.go"))
    if test_files:
        raise ValueError("Go-compatible coverage unexpectedly contains test files: " + ", ".join(test_files))

    print(f"verified LLGo coverage profile: {len(files)} source files; both LLGo-only probes executed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
