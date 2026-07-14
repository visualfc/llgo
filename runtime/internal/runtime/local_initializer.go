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

import "unsafe"

const (
	localInitUninitialized uint8 = iota
	localInitInitializing
	localInitReady
	localInitFailed
)

// EnsureLocalInitializer executes one package/locality dispatcher at most once
// in the current owner. Recursive access observes partial initialization; a
// recovered failure remains failed and re-panics on every later access.
func EnsureLocalInitializer(state *uint8, failureKey unsafe.Pointer, initialize func()) {
	switch *state {
	case localInitReady:
		return
	case localInitInitializing:
		return
	case localInitFailed:
		panic(*localInitializerFailure(failureKey))
	case localInitUninitialized:
	default:
		panic("runtime: invalid local initializer state")
	}
	*state = localInitInitializing
	completed := false
	defer func() {
		if completed {
			return
		}
		value := recover()
		*localInitializerFailure(failureKey) = value
		*state = localInitFailed
		panic(value)
	}()
	initialize()
	completed = true
	*state = localInitReady
}

func localInitializerFailure(key unsafe.Pointer) *any {
	var value any
	return (*any)(LocalPackage(key, unsafe.Sizeof(value), unsafe.Alignof(value)))
}
