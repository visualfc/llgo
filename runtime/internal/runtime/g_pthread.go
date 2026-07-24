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
	"unsafe"

	c "github.com/goplus/llgo/runtime/internal/clite"
	"github.com/goplus/llgo/runtime/internal/clite/pthread"
)

var gKey = newGKey()

func newGKey() pthread.Key {
	var key pthread.Key
	if ret := key.Create(pthread.KeyDestructor(destroyG)); ret != 0 {
		c.Fprintf(c.Stderr, c.Str("runtime: pthread_key_create failed (errno=%d)\n"), ret)
		panic("runtime: failed to create getg key")
	}
	return key
}

func getg() *g {
	if ptr := gKey.Get(); ptr != nil {
		return (*g)(ptr)
	}
	ptr := AllocRoot(unsafe.Sizeof(g{}))
	if ptr == nil {
		panic("runtime: failed to allocate g")
	}
	c.Memset(ptr, 0, unsafe.Sizeof(g{}))
	if ret := gKey.Set(ptr); ret != 0 {
		FreeRoot(ptr)
		c.Fprintf(c.Stderr, c.Str("runtime: pthread_setspecific failed (errno=%d)\n"), ret)
		panic("runtime: failed to install g")
	}
	return (*g)(ptr)
}

func destroyG(ptr c.Pointer) {
	gp := (*g)(ptr)
	if gp == nil {
		return
	}
	if gp.panic_ != nil {
		c.Free(gp.panic_)
	}
	*gp = g{}
	FreeRoot(ptr)
}
