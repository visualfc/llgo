//go:build llgo && wasip1 && wasm && llgo.wasi_threads && llgo.wasm.gc.linear

package sync

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

//go:linkname wasiGCCondTimedWait github.com/xgo-dev/llgo/runtime/internal/runtime.WasiGCCondTimedWait
func wasiGCCondTimedWait(cond, mutex unsafe.Pointer, waitNanos int64, monotonic bool)

//go:linkname mutexLock github.com/xgo-dev/llgo/runtime/internal/runtime.wasiGCMutexLock
func mutexLock(m *Mutex)

func condWait(cond *Cond, mutex *Mutex) c.Int {
	wasiGCCondTimedWait(unsafe.Pointer(cond), unsafe.Pointer(mutex), 20*1e6, false)
	return 0
}
