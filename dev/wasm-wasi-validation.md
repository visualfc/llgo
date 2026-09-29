# WAMR threads and exception validation

The W32 backend keeps one goroutine per pthread. The following checks qualify
that backend; they do not introduce an M:N scheduler on native or WASI targets.

## Fixed failure modes

- WAMR 2.4.5's classic interpreter briefly broadcast thread termination while
  propagating a catchable exception to a Wasm caller. The interpreter patch
  keeps that propagation local. The original deferred-Goexit artifact passed
  40/50 runs on stock WAMR and 100/100 with the patch. Escaping exceptions still
  fail the invocation. Encoding comparisons and the supported Go/C++ boundary
  are recorded in [wasm-eh-comparison.md](wasm-eh-comparison.md).
- A pthread condition wait can block while reacquiring a mutex held by a Go
  thread already stopped for collection. Merely polling the condition every
  20 ms did not resolve that cycle: finalizer tests repeatedly skipped GC and
  exceeded their three-second deadlines. Runtime mutex/condition waits now
  publish the suspended Go caller's roots before entering C and prevent a
  return to Go until collection finishes. This includes the timer scheduler.
  Arbitrary user C calls retain the bounded-wait/skip-collection behavior.
- The initial thread parked after Goexit must unregister its roots, since it
  will never execute another Go safepoint. A worker now verifies that GC still
  advances after initial Goexit with the timer service active.
- Repeated explicit GC must allow waiting allocations and resumed threads to
  progress. The allocator uses the same GC-safe pthread mutex instead of
  spinning through repeated host calls, and the next collection waits for the
  previous rendezvous to finish. Compiler polls and runtime symbol-table
  initialization waiters acknowledge collection requests. Arbitrary user C
  calls can still prevent collection. The 32-thread/1000-round concurrent
  function-info test timed out at 100 seconds with the spin lock; the blocking
  allocator completed it in 10.72 seconds (nogc comparison: 7.26 seconds) on
  macOS arm64. These are local regression timings, not cross-host benchmarks.
- Small programs begin with a 1 MiB GC arena; subsequent arenas double up to
  32 MiB. This avoids sweeping a nearly empty 32 MiB arena on every explicit
  collection. WAMR is built in Release mode, with the same classic interpreter,
  exception and thread features used by the acceptance tests.
- Reflection's whole-program type-name lookup compares encoded names without
  allocating a temporary string for each extra-star type. This preserves the
  existing type identity and name rules while reducing GC contention.

## Reproducible checks

```sh
bash dev/build_iwasm.sh
go test ./internal/build -run '^TestWASIGCWaitsSuspendGoAcrossMutexReacquisition$'
LLGO="$PWD/.bin/llgo" IWASM=/path/to/patched/iwasm python3 dev/test_wasm_wasi_threads.py
LLGO_WASI_THREADS=1 go run ./dev/wasmstdlib -full -profile W32-WASI \
  -shard 9 -shards 16 -llgo "$PWD/.bin/llgo" -report w32-shard9.json
```

The pthread regression exercises both mutex waits and condition-variable
reacquisition, including a waiter woken while collection is still active.
The WAMR acceptance driver covers GC/nogc exception isolation, main/init/worker
Goexit, finalizers, reflection with concurrent GC, retained roots, heap growth,
uncooperative C, timers, filesystems, selected standard-library tests and a
GOROOT sentinel. The full package audit remains a separate gate; passing this
focused suite is not a claim of complete standard-library compatibility.
