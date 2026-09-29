# W32 WASI threads

`llgo build/run/test -target wasi` (including the `wasip1` alias and raw
`GOOS=wasip1 GOARCH=wasm`) uses wasi-libc's pthread startup, shared linear
memory, and the threaded linear collector by default. WASI output no longer
runs Binaryen Asyncify. Native and embedded pthread backends are unchanged.

WAMR is the first supported runner. Build the pinned, patched interpreter with
`bash dev/build_iwasm.sh`, and add the printed cache `bin` directory to `PATH`.
The public run/test commands configure WAMR's thread limit, stack, and preopens.
The runner grants the absolute package working directory and `/tmp` rather than
the entire host filesystem. The GOROOT comparison runs both W32 artifacts with WAMR. Only the separate
`dev/wasmstdlib` official-Go reference profile uses Wasmtime; that does not imply
support for executing LLGo's threaded W32 artifact there.

The former `LLGO_WASI_THREADS=1` opt-in is unnecessary and remains accepted.
Setting it to `0`/`false`/`off` now produces a migration error instead of silently
building a different runtime. The `llgo.wasi_threads` source tag remains part
of compilation/cache identity. `-tags nogc` is still available; the default
enables GC. Single-thread WASI context switching and its allocator wrappers
have been removed.

WASI retains one goroutine per host pthread. It does not use the browser's
bounded worker scheduler, and `LLGO_WASM_WORKERS` remains a browser-only option.
Host thread and shared-memory limits therefore bound the number of concurrent
WASI goroutines. A C call that cannot acknowledge a safepoint causes that
collection attempt to be skipped; the collector does not scan an actively
mutating foreign stack.

Run `python3 dev/test_wasm_wasi_threads.py` for pthread/GC, EH, filesystem,
run/test, selected standard-library, and GOROOT checks. Run
`python3 dev/test_wasm_debug_info.py --profile w32` to validate final Go/C++
DWARF and source-line mapping at O0/O2. `dev/test_wasm_target_profiles.sh`
covers named and raw profile builds. Full compatibility audit results are
reported separately and must not be inferred from these focused checks.

This default switch depends on the WAMR stability and EH work in #2695, which
in turn depends on #2669. Browser filesystem integration is tracked in #2696.
WasmGC, W64, JSPI, and WASI Preview 2/components remain separate proposals.
