# Native source-debug acceptance

Run `bash cmd/llgo/debugtest/native/runtest.sh` with the current `llgo` in PATH.
The runner uses default native DWARF at O0 and O2 and fails if any LLDB Python
assertion fails. `LLGO_LLDB` selects an installed LLDB.

The O0 cases require the shared runtime panic entry and the exact Go caller
line for explicit panic, integer division by zero, and invalid memory access.
The O2 case requires leaf and middle inline frames in source order, an actual
noinline caller, and step-over reaching the caller's following source line.
Go/C/Go callbacks and host fault stacks are covered by the existing
`cmd/llgo/lldbtest/mixed` suite.
