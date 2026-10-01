# Debugger acceptance tests

This directory contains integration suites that invoke real debugger tools.
They run explicitly in CI and require an installed LLGo compiler and the tools
listed by each suite.

- [Runtime inspection](runtime/README.md) shares native Go and C fixtures between
  LLDB and GDB, covering variables, runtime values, goroutines, and backtraces.

Run the native LLDB suite from the repository root:

```sh
bash test/debug/runtime/runtest.sh -v
```

Run the GDB suite on a supported native host with Python-enabled GDB 12+
(the Windows ARM64 jobs use GDB 18+):

```sh
LLGO_GDB_INTEGRATION=1 go test ./cmd/internal/gdb -run '^TestGDBIntegration$' -count=1 -v
```

The runtime fixtures retain their own `go.mod` and the `lldbtest` module name
used by debugger type assertions. The nested module excludes these programs
from root-module `go test ./test/...` and `llgo test ./test/...` enumeration.
The LLGo workflow invokes the LLDB suite in its native platform jobs and the
GDB suite in its Linux and all six Windows ABI/architecture jobs. A separate
Intel macOS job exercises both debuggers; its GDB wrapper uses the ephemeral
runner's passwordless sudo permission to obtain native task ports.
The Windows debugger is installed separately from the pinned compiler and
LLDB dependencies. Apple Silicon has no native GDB process target: use LLDB
there; GDB remote debugging remains available. Unit tests and package-specific `testdata` remain
beside their implementation.
