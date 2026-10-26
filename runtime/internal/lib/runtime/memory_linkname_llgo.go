package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	"github.com/xgo-dev/llgo/runtime/internal/sync/atomic"
)

const memoryHookWordSize = unsafe.Sizeof(uintptr(0))

// External packages use these Go runtime hooks through go:linkname, including
// taking their addresses. Keep wrappers with the Go signatures instead of
// exposing the C functions, which return a pointer.

//go:linkname memmove runtime.memmove
//go:nosplit
func memmove(dst, src unsafe.Pointer, n uintptr) {
	if n == 0 {
		return
	}
	if (uintptr(dst)|uintptr(src)|n)&(memoryHookWordSize-1) == 0 {
		// Go requires indivisible pointer-sized writes for aligned ranges.
		// Unordered stores preserve that guarantee without synchronization
		// fences, and prevent LLVM from introducing tearing stores or libc
		// calls. The contract applies to large copies too, so keep this path
		// for every aligned size. Source reads need no atomic ordering.
		// Copy backward when dst overlaps the source at a higher address.
		if uintptr(dst) > uintptr(src) && uintptr(dst)-uintptr(src) < n {
			for n != 0 {
				n -= memoryHookWordSize
				value := *(*uintptr)(unsafe.Add(src, n))
				atomic.StoreUnordered((*uintptr)(unsafe.Add(dst, n)), value)
			}
		} else {
			for offset := uintptr(0); offset < n; offset += memoryHookWordSize {
				value := *(*uintptr)(unsafe.Add(src, offset))
				atomic.StoreUnordered((*uintptr)(unsafe.Add(dst, offset)), value)
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
	if (uintptr(ptr)|n)&(memoryHookWordSize-1) == 0 {
		// This hook also underlies pointer-bearing clears in Go, so aligned
		// pointer-sized words must become zero in a single store at every size.
		// Unordered stores provide atomicity without synchronization fences.
		for offset := uintptr(0); offset < n; offset += memoryHookWordSize {
			atomic.StoreUnordered((*uintptr)(unsafe.Add(ptr, offset)), uintptr(0))
		}
		return
	}
	c.Memset(ptr, 0, n)
}
