//go:build llgo && !baremetal

/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
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
	"sync/atomic"
	"unsafe"

	"github.com/goplus/llgo/runtime/internal/clite/pthread"
)

type poolTLSHandle struct {
	key    pthread.Key
	victim *unsafe.Pointer
}

type poolTLSSlot struct {
	handle *poolTLSHandle
	local  unsafe.Pointer
}

//go:linkname syncPoolLocalAlloc sync.runtime_poolLocalAlloc
func syncPoolLocalAlloc(victim *unsafe.Pointer) unsafe.Pointer {
	handle := &poolTLSHandle{victim: victim}
	if ret := handle.key.Create(pthread.KeyDestructor(destroyPoolTLSSlot)); ret != 0 {
		panic("runtime: failed to create sync.Pool TLS key")
	}
	return unsafe.Pointer(handle)
}

//go:linkname syncPoolLocalGet sync.runtime_poolLocalGet
func syncPoolLocalGet(raw unsafe.Pointer) unsafe.Pointer {
	handle := (*poolTLSHandle)(raw)
	if slot := (*poolTLSSlot)(handle.key.Get()); slot != nil {
		return slot.local
	}
	return nil
}

//go:linkname syncPoolLocalSet sync.runtime_poolLocalSet
func syncPoolLocalSet(raw, local unsafe.Pointer) {
	handle := (*poolTLSHandle)(raw)
	if slot := (*poolTLSSlot)(handle.key.Get()); slot != nil {
		slot.local = local
		return
	}
	slot := (*poolTLSSlot)(AllocRoot(unsafe.Sizeof(poolTLSSlot{})))
	if slot == nil {
		panic("runtime: failed to allocate sync.Pool TLS slot")
	}
	slot.handle = handle
	slot.local = local
	if ret := handle.key.Set(unsafe.Pointer(slot)); ret != 0 {
		slot.handle = nil
		slot.local = nil
		FreeRoot(unsafe.Pointer(slot))
		panic("runtime: failed to install sync.Pool TLS slot")
	}
}

func destroyPoolTLSSlot(raw unsafe.Pointer) {
	slot := (*poolTLSSlot)(raw)
	if slot == nil {
		return
	}
	handle := slot.handle
	if handle != nil && handle.victim != nil {
		atomic.StorePointer(handle.victim, slot.local)
	}
	slot.handle = nil
	slot.local = nil
	FreeRoot(raw)
}
