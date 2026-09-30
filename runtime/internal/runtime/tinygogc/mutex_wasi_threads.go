//go:build llgo && wasip1 && wasm && llgo.wasi_threads && llgo.wasm.gc.linear

package tinygogc

import "github.com/xgo-dev/llgo/runtime/internal/sync"

// wasi-libc's PTHREAD_MUTEX_INITIALIZER is all zero, so the global allocator
// lock is usable before Go package initialization. Runtime Mutex.Lock publishes
// roots while waiting in C, letting the collector stop allocation waiters.
// Sleeping on the host futex avoids a sched_yield/host-mutex polling storm
// when many pthreads allocate concurrently in WAMR's interpreter.
type mutex = sync.Mutex

func lock(m *mutex) { m.Lock() }

func unlock(m *mutex) { m.Unlock() }
