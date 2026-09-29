//go:build llgo && wasip1 && wasm && llgo.wasi_threads && llgo.wasm.gc.linear

package tinygogc

import (
	_ "unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	"github.com/xgo-dev/llgo/runtime/internal/sync/atomic"
)

type mutex struct{ next, serving uint32 }

func lock(m *mutex) {
	// atomic.Add returns the pre-increment value. Hand ownership to waiters
	// in order so a GC loop cannot overtake a resumed allocation repeatedly.
	ticket := atomic.Add(&m.next, uint32(1))
	for atomic.Load(&m.serving) != ticket {
		wasiGCAllocatorYield()
		_ = wasiThreadYield()
	}
}

func unlock(m *mutex) { atomic.Add(&m.serving, uint32(1)) }

//go:linkname wasiGCAllocatorYield github.com/xgo-dev/llgo/runtime/internal/runtime.wasiGCSafepoint
func wasiGCAllocatorYield()

//go:linkname wasiThreadYield C.sched_yield
func wasiThreadYield() c.Int
