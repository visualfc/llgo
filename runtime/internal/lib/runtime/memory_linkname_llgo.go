package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	"github.com/xgo-dev/llgo/runtime/internal/sync/atomic"
)

// External packages use these Go runtime hooks through go:linkname, including
// taking their addresses. Keep wrappers with the Go signatures instead of
// exposing the C functions, which return a pointer.

//go:linkname memmove runtime.memmove
//go:nosplit
func memmove(dst, src unsafe.Pointer, n uintptr) {
	if n == 0 {
		return
	}
	word := unsafe.Sizeof(uintptr(0))
	if (uintptr(dst)|uintptr(src)|n)&(word-1) == 0 {
		// Go requires indivisible pointer-sized writes for aligned ranges.
		// Atomic operations also prevent LLVM from merging these stores into
		// a libc call with weaker guarantees. Copy backward when dst overlaps
		// the source at a higher address.
		if uintptr(dst) > uintptr(src) && uintptr(dst)-uintptr(src) < n {
			for n != 0 {
				n -= word
				value := atomic.Load((*uintptr)(unsafe.Add(src, n)))
				atomic.Store((*uintptr)(unsafe.Add(dst, n)), value)
			}
		} else {
			for offset := uintptr(0); offset < n; offset += word {
				value := atomic.Load((*uintptr)(unsafe.Add(src, offset)))
				atomic.Store((*uintptr)(unsafe.Add(dst, offset)), value)
			}
		}
		return
	}
	c.Memmove(dst, src, n)
}

//go:linkname memclrNoHeapPointers runtime.memclrNoHeapPointers
//go:nosplit
func memclrNoHeapPointers(ptr unsafe.Pointer, n uintptr) {
	if n == 0 {
		return
	}
	word := unsafe.Sizeof(uintptr(0))
	if (uintptr(ptr)|n)&(word-1) == 0 {
		// This hook also underlies pointer-bearing clears in Go, so aligned
		// pointer-sized words must become zero in a single store.
		for offset := uintptr(0); offset < n; offset += word {
			atomic.Store((*uintptr)(unsafe.Add(ptr, offset)), uintptr(0))
		}
		return
	}
	c.Memset(ptr, 0, n)
}
