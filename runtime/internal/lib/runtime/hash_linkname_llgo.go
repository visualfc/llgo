package runtime

import (
	"unsafe"

	llruntime "github.com/xgo-dev/llgo/runtime/internal/runtime"
)

// These symbols are referenced by hash/maphash and other standard-library
// packages through runtime linknames.

//go:linkname runtime_memhash runtime.memhash
func runtime_memhash(p unsafe.Pointer, seed, size uintptr) uintptr {
	return llruntime.Memhash(p, seed, size)
}

//go:linkname runtime_memhash32 runtime.memhash32
func runtime_memhash32(p unsafe.Pointer, seed uintptr) uintptr {
	return llruntime.Memhash32(p, seed)
}

//go:linkname runtime_memhash64 runtime.memhash64
func runtime_memhash64(p unsafe.Pointer, seed uintptr) uintptr {
	return llruntime.Memhash64(p, seed)
}
