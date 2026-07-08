# Manual caller-info acceptance playground

Each directory runs under both `go` and `llgo` for side-by-side comparison.
Every scenario also has an automated regression in `test/go`
(`caller_acceptance_test.go`); this playground exists for eyeballing real
output. From the repository root:

    export LLGO_ROOT=$(git rev-parse --show-toplevel)
    go build -o /tmp/llgo ./cmd/llgo    # or: go run ./cmd/llgo ...

Conformance bar: output format and user-code file:line match gc exactly;
runtime-internal, patched-stdlib and startup frames may differ.

## panic — Go-style traceback for an unrecovered panic
    cd test/_manualtest/panic
    go run .            # gc's goroutine traceback
    /tmp/llgo run .     # same shape: names + file:line + offset (exit 2)

## logging — log.Lshortfile / slog AddSource
    cd test/_manualtest/logging
    go run . && /tmp/llgo run .    # the main.go:NN locations must agree

## callers — Caller ladder / CallersFrames / FuncForPC panorama
    cd test/_manualtest/callers
    go run . > /tmp/gc.txt; /tmp/llgo run . > /tmp/llgo.txt
    diff /tmp/gc.txt /tmp/llgo.txt
    # line columns must agree; known diffs: runtime-internal frame lines,
    # gc's runtime.main/goexit tail frames

## testfail — llgo test failure locations
    cd test/_manualtest/testfail
    go test .           # x_test.go:NN: boom
    /tmp/llgo test .    # the same x_test.go:NN: boom

## cexcept — hardware faults in C code called from Go
    cd test/_manualtest/cexcept
    /tmp/llgo run . segv recover     # NULL store in C: recover works, prints
                                     # the post-recover stack
    /tmp/llgo run . segv norecover   # unrecovered: panic + gc-style traceback
                                     # (fault-site frames pending the
                                     # panic-snapshot follow-up)
    /tmp/llgo run . div recover      # arm64 integer division does not trap
                                     # (hardware returns 0); amd64 raises
                                     # SIGFPE

Verified (darwin/arm64 + linux/arm64 + linux/amd64):
- SIGSEGV in a C frame converts to a Go panic; recover observes gc's exact
  error text.
- Known gaps (recorded for the follow-up PRs):
  1. The fault-site stack (cexc_leaf_segv -> cexc_mid_segv x3 -> cexc_segv
     -> Go frames) is not visible yet — recover/tracebacks show the
     post-longjmp stack. The panic-snapshot follow-up extends to signal
     handlers: walk the FP chain from the ucontext pc/fp; C frames get
     dladdr names, Go frames funcinfo names — same machinery as the
     unwinder.
  2. Only SIGSEGV is installed; SIGFPE (amd64 division) core-dumps, SIGBUS
     is not handled.
  3. No sigaltstack: on stack overflow the handler cannot run and the
     process dies (gc prints "stack overflow"). Note that C-side UB gets
     propagated by clang (this test was once optimized into infinite
     recursion); wrap/fault.c uses a volatile pointer to prevent that.
