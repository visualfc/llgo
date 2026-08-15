package runtime

import (
	"sync/atomic"
	"unsafe"

	"github.com/goplus/llgo/runtime/internal/clite/tls"
)

//go:linkname sync_runtime_poolLocalAlloc sync.runtime_poolLocalAlloc
func sync_runtime_poolLocalAlloc(victim *unsafe.Pointer) unsafe.Pointer {
	handle := tls.Alloc[unsafe.Pointer](func(local *unsafe.Pointer) {
		if local != nil {
			atomic.StorePointer(victim, *local)
		}
	})
	return unsafe.Pointer(&handle)
}

//go:linkname sync_runtime_poolLocalGet sync.runtime_poolLocalGet
func sync_runtime_poolLocalGet(handle unsafe.Pointer) unsafe.Pointer {
	return (*tls.Handle[unsafe.Pointer])(handle).Get()
}

//go:linkname sync_runtime_poolLocalSet sync.runtime_poolLocalSet
func sync_runtime_poolLocalSet(handle, local unsafe.Pointer) {
	(*tls.Handle[unsafe.Pointer])(handle).Set(local)
}
