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

package tls

import (
	"sync/atomic"
	"unsafe"
)

//go:linkname sync_poolLocalAlloc sync.runtime_poolLocalAlloc
func sync_poolLocalAlloc(victim *unsafe.Pointer) unsafe.Pointer {
	handle := Alloc[unsafe.Pointer](func(local *unsafe.Pointer) {
		if local != nil {
			atomic.StorePointer(victim, *local)
		}
	})
	return unsafe.Pointer(&handle)
}

//go:linkname sync_poolLocalGet sync.runtime_poolLocalGet
func sync_poolLocalGet(handle unsafe.Pointer) unsafe.Pointer {
	return (*Handle[unsafe.Pointer])(handle).Get()
}

//go:linkname sync_poolLocalSet sync.runtime_poolLocalSet
func sync_poolLocalSet(handle, local unsafe.Pointer) {
	(*Handle[unsafe.Pointer])(handle).Set(local)
}
