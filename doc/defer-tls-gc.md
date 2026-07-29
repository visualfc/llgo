# Defer Locality and GC Integration

## Background

`defer` chains belong to the current goroutine, and unwind paths must locate the
active `*runtime.Defer`. A raw pointer stored only in pthread TLS is invisible to
the Boehm collector. In stress scenarios—e.g. `TestDeferLoopStress` with
1,000,000 defers—the collector could reclaim defer nodes, leaving dangling
pointers and causing crashes inside deferred closures.

Prior experiments (`test-defer-dont-free` branch) confirmed the crash disappeared when allocations bypassed GC (plain `malloc` without `free`), pointing to a root-registration gap rather than logical corruption.

## Current Design

1. **Runtime-owned goroutine state**
   - Each `runtimeContext` owns its `g`, `m`, and `p`; `g.defer_` is the current
     goroutine's defer-chain head.
   - The context is allocated with `AllocRoot`, so pointers reachable through
     `g` remain visible to the collector.
   - A pointer-free `//llgo:tls` `uintptr` slot locates the current `g`. The
     slot is only an address cache; the `runtimeContext` allocation is the GC
     root.
   - Bare-metal targets keep the same state in ordinary globals because they
     have one execution context and their LLVM backends do not support native
     TLS relocations.
   - Runtime-created threads release that root in `mexit`. A pthread key remains
     only as a thread-exit destructor sidecar for main and foreign threads; the
     current-G read path does not call `pthread_getspecific`.

2. **SSA codegen synchronization**
   - `ssa/eh.go` calls `runtime.SetThreadDefer` whenever it updates the current
     goroutine's defer head (on first allocation and when restoring the
     previous link during unwind).
   - Defer argument nodes and the `runtime.Defer` struct itself are allocated with `aggregateAllocU`, ensuring new memory comes from GC-managed heaps, and nodes are released via `runtime.FreeDeferNode`.

3. **Locality directives**
   - Runtime state that belongs to a logical goroutine uses `//llgo:gls`.
   - Physical scheduler and execution-resource slots that must be available
     before goroutine-local context setup use `//llgo:tls`.
   - Pointer-bearing locality variables are kept in the GC-rooted locality
     package payload rather than bespoke pthread-key allocations.
   - Bare-metal builds use ordinary globals for the caller and FIPS state:
     they have one logical context, and avoiding the locality package cache
     keeps unsupported native TLS relocations out of those targets.

4. **Dynamic `sync.Pool` state**
   - `sync.Pool` retains the GC-aware `clite/tls` handle. Each `Pool` needs a
     dynamically allocated per-thread slot, while locality directives declare
     a fixed set of static package variables.
   - Keeping the handle also preserves the atomic hot path, thread-exit victim
     handoff, cross-thread stealing, and cleanup of short-lived pools. A static
     TLS map would retain every `*Pool` for the lifetime of a thread.

5. **Non-GC builds**
   - `FreeDeferNode` continues to release nodes via `c.Free` when building with
     `-tags nogc`.

## Testing

Run the stress and regression suites to validate the integration:

```sh
./dev/llgo.sh test ./test -run TestDeferLoopStress
./dev/llgo.sh test ./test
```

The updated `TestDeferLoopStress` now asserts 1,000,000 loop defers execute without failure, catching regressions in GC root tracking.
