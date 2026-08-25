//go:build darwin || linux || windows

package runtime

import (
	"sync/atomic"
	"unsafe"
)

// LLGo's conservative collector does not require Go write barriers, so these
// hooks can use the standard library's architecture-specific uintptr atomics.

//go:linkname atomic_storePointer internal/runtime/atomic.storePointer
func atomic_storePointer(ptr *unsafe.Pointer, new unsafe.Pointer) {
	atomic.StoreUintptr((*uintptr)(unsafe.Pointer(ptr)), uintptr(new))
}

//go:linkname atomic_casPointer internal/runtime/atomic.casPointer
func atomic_casPointer(ptr *unsafe.Pointer, old, new unsafe.Pointer) bool {
	return atomic.CompareAndSwapUintptr(
		(*uintptr)(unsafe.Pointer(ptr)), uintptr(old), uintptr(new),
	)
}
