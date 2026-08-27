package runtime

import (
	"unsafe"

	"github.com/xgo-dev/llgo/runtime/abi"
	"github.com/xgo-dev/llgo/runtime/internal/runtime"
)

type eface struct {
	_type *abi.Type
	data  unsafe.Pointer
}

func typeOf(v any) *abi.Type {
	return (*eface)(unsafe.Pointer(&v))._type
}

// AddCleanup attaches a cleanup function to ptr. Some time after ptr is no longer
// reachable, the runtime will call cleanup(arg) in a separate goroutine.
//
// AddCleanup panics if ptr is nil or if arg equals ptr.
//
// The returned Cleanup handle can be used to cancel the cleanup before it runs.
func AddCleanup[T, S any](ptr *T, cleanup func(S), arg S) Cleanup {
	// The pointer to the object must be valid.
	if ptr == nil {
		panic("runtime.AddCleanup: ptr is nil")
	}
	// Check that arg is not equal to ptr.
	if kind := typeOf(arg).Kind(); kind == abi.Pointer || kind == abi.UnsafePointer {
		if unsafe.Pointer(ptr) == *((*unsafe.Pointer)(unsafe.Pointer(&arg))) {
			panic("runtime.AddCleanup: ptr is equal to arg, cleanup will never run")
		}
	}
	fn := func() {
		cleanup(arg)
	}
	id := runtime.AddCancelableCleanupPtr(unsafe.Pointer(ptr), fn)
	return Cleanup{id: id}
}

type Cleanup struct {
	id uint64
	// Keep Go's second word for type-layout compatibility, but leave it zero:
	// BDWGC conservatively treats a uintptr containing ptr as a live root.
	ptr uintptr
}

func (c Cleanup) Stop() {
	runtime.StopCleanupPtr(c.id)
}
