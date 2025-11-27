package runtime

import (
	"unsafe"

	"github.com/goplus/llgo/runtime/abi"
	"github.com/goplus/llgo/runtime/internal/runtime"
)

type eface struct {
	_type *abi.Type
	data  unsafe.Pointer
}

func typeOf(v any) *abi.Type {
	return (*eface)(unsafe.Pointer(&v))._type
}

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
	return Cleanup{runtime.AddCleanupPtr(unsafe.Pointer(ptr), fn)}
}

type Cleanup struct {
	cancel func()
}

func (c Cleanup) Stop() {
	c.cancel()
}
