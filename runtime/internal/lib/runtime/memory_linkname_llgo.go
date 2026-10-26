package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

// External packages use these Go runtime hooks through go:linkname, including
// taking their addresses. Keep wrappers with the Go signatures instead of
// exposing the C functions, which return a pointer.

//go:linkname memmove runtime.memmove
//go:nosplit
func memmove(dst, src unsafe.Pointer, n uintptr) {
	if n != 0 {
		c.Memmove(dst, src, n)
	}
}

//go:linkname memclrNoHeapPointers runtime.memclrNoHeapPointers
//go:nosplit
func memclrNoHeapPointers(ptr unsafe.Pointer, n uintptr) {
	if n != 0 {
		c.Memset(ptr, 0, n)
	}
}
