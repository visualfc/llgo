//go:build !nogc && !baremetal

/*
 * Copyright (c) 2024 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	"github.com/xgo-dev/llgo/runtime/internal/clite/bdwgc"
	psync "github.com/xgo-dev/llgo/runtime/internal/sync"
	"github.com/xgo-dev/llgo/runtime/internal/sync/atomic"
)

// AllocU allocates uninitialized memory.
func AllocU(size uintptr) unsafe.Pointer {
	ret := bdwgc.Malloc(size)
	recordMemProfileAlloc(size)
	return ret
}

// AllocZ allocates zero-initialized memory.
func AllocZ(size uintptr) unsafe.Pointer {
	ret := bdwgc.Malloc(size)
	recordMemProfileAlloc(size)
	return c.Memset(ret, 0, size)
}

func AllocRoot(size uintptr) unsafe.Pointer {
	return bdwgc.MallocUncollectable(size)
}

func FreeRoot(ptr unsafe.Pointer) {
	bdwgc.Free(ptr)
}

type entry struct {
	fn   func()         // cleanup func
	prev unsafe.Pointer // prev cleanup func ptr
	id   uint64         // non-zero for a Cleanup handle
	stop int32
}

var cancelableCleanupState struct {
	once    psync.Once
	mu      psync.Mutex
	nextID  uint64
	entries map[uint64]*entry
}

func initCancelableCleanupState() {
	cancelableCleanupState.mu.Init(nil)
	cancelableCleanupState.entries = make(map[uint64]*entry)
}

func takeCancelableCleanup(e *entry) bool {
	if e.id == 0 {
		return atomic.Load(&e.stop) != 1
	}
	cancelableCleanupState.mu.Lock()
	run := cancelableCleanupState.entries[e.id] == e
	if run {
		delete(cancelableCleanupState.entries, e.id)
	}
	cancelableCleanupState.mu.Unlock()
	return run
}

func finalizer(ptr unsafe.Pointer, cb unsafe.Pointer) {
	e := (*entry)(cb)
	if ptr := atomic.Load(&e.prev); ptr != nil {
		(*(*func())(ptr))()
	}
	if takeCancelableCleanup(e) {
		e.fn()
	}
}

func registerCleanupPtr(ptr unsafe.Pointer, e *entry) {
	var oldFn bdwgc.FinalizerFunc
	var oldCb unsafe.Pointer
	bdwgc.RegisterFinalizer(ptr, finalizer, unsafe.Pointer(e), &oldFn, &oldCb)
	if oldCb != nil {
		n := uintptr(ptr) ^ 0xffff // hides the pointer from escape analysis
		fn := func() {
			oldFn((unsafe.Pointer)(n^0xffff), oldCb)
		}
		atomic.Store(&e.prev, unsafe.Pointer(&fn))
	}
}

// AddCleanupPtr attaches a cleanup function to ptr. Some time after ptr is no longer
// reachable, the runtime will call cleanup().
func AddCleanupPtr(ptr unsafe.Pointer, cleanup func()) (cancel func()) {
	e := &entry{fn: cleanup}
	registerCleanupPtr(ptr, e)
	return func() {
		atomic.Store(&e.stop, 1)
	}
}

// AddCancelableCleanupPtr registers a cleanup and returns a stable, pointer-free
// identifier suitable for runtime.Cleanup's Go-compatible representation.
func AddCancelableCleanupPtr(ptr unsafe.Pointer, cleanup func()) uint64 {
	cancelableCleanupState.once.Do(initCancelableCleanupState)
	cancelableCleanupState.mu.Lock()
	var id uint64
	for id == 0 || cancelableCleanupState.entries[id] != nil {
		cancelableCleanupState.nextID++
		id = cancelableCleanupState.nextID
	}
	e := &entry{fn: cleanup, id: id}
	cancelableCleanupState.entries[id] = e
	cancelableCleanupState.mu.Unlock()
	registerCleanupPtr(ptr, e)
	return id
}

// StopCleanupPtr cancels a pending cleanup. If its finalizer has already
// claimed the entry, Stop has no effect, matching runtime.Cleanup.Stop.
func StopCleanupPtr(id uint64) {
	if id == 0 {
		return
	}
	cancelableCleanupState.once.Do(initCancelableCleanupState)
	cancelableCleanupState.mu.Lock()
	if e := cancelableCleanupState.entries[id]; e != nil {
		atomic.Store(&e.stop, 1)
		delete(cancelableCleanupState.entries, id)
	}
	cancelableCleanupState.mu.Unlock()
}
