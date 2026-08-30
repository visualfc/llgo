package runtime

import (
	"unsafe"

	latomic "sync/atomic"
	_ "unsafe"

	llrt "github.com/xgo-dev/llgo/runtime/internal/runtime"
	psync "github.com/xgo-dev/llgo/runtime/internal/sync"
)

type weakHandle struct {
	key  uintptr
	live uint32
}

// BDWGC conservatively treats pointer-looking uintptr values as live roots.
// Keep weak-pointer identities encoded everywhere they outlive the call that
// registers them, including map keys and weak handles.
func encodeWeakPointer(p unsafe.Pointer) uintptr {
	return ^uintptr(p)
}

func decodeWeakPointer(key uintptr) unsafe.Pointer {
	return unsafe.Pointer(^key)
}

var weakState struct {
	once psync.Once
	mu   psync.Mutex
	m    map[uintptr]*weakHandle
}

func initWeakState() {
	weakState.mu.Init(nil)
	weakState.m = make(map[uintptr]*weakHandle)
}

func llgoRegisterWeakPointer(p unsafe.Pointer) unsafe.Pointer {
	if p == nil {
		return nil
	}
	weakState.once.Do(initWeakState)

	key := encodeWeakPointer(p)
	weakState.mu.Lock()
	if h := weakState.m[key]; h != nil {
		weakState.mu.Unlock()
		return unsafe.Pointer(h)
	}
	h := &weakHandle{key: key, live: 1}
	weakState.m[key] = h
	weakState.mu.Unlock()

	// Keep the cleanup closure limited to encoded identities. Capturing p here
	// would turn the cleanup itself into a strong reference to the referent.
	llrt.AddCleanupPtr(p, func() {
		latomic.StoreUint32(&h.live, 0)
		weakState.mu.Lock()
		if weakState.m[key] == h {
			delete(weakState.m, key)
		}
		weakState.mu.Unlock()
	})
	return unsafe.Pointer(h)
}

func llgoMakeStrongFromWeak(u unsafe.Pointer) unsafe.Pointer {
	h := (*weakHandle)(u)
	if h == nil || latomic.LoadUint32(&h.live) == 0 {
		return nil
	}
	return decodeWeakPointer(h.key)
}

//go:linkname weak_runtime_registerWeakPointer weak.runtime_registerWeakPointer
func weak_runtime_registerWeakPointer(p unsafe.Pointer) unsafe.Pointer {
	return llgoRegisterWeakPointer(p)
}

//go:linkname weak_runtime_makeStrongFromWeak weak.runtime_makeStrongFromWeak
func weak_runtime_makeStrongFromWeak(u unsafe.Pointer) unsafe.Pointer {
	return llgoMakeStrongFromWeak(u)
}

//go:linkname internal_weak_runtime_registerWeakPointer internal/weak.runtime_registerWeakPointer
func internal_weak_runtime_registerWeakPointer(p unsafe.Pointer) unsafe.Pointer {
	return llgoRegisterWeakPointer(p)
}

//go:linkname internal_weak_runtime_makeStrongFromWeak internal/weak.runtime_makeStrongFromWeak
func internal_weak_runtime_makeStrongFromWeak(u unsafe.Pointer) unsafe.Pointer {
	return llgoMakeStrongFromWeak(u)
}
