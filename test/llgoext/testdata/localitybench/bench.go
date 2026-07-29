//go:build llgo

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

package localitybench

import "unsafe"

var backing int

var ordinaryPointer *int

//llgo:tls
var nativePointerBits uintptr

//llgo:gls
var pointer *int

//go:noinline
func Touch() {
	pointer = &backing
}

func PrepareReads() {
	ordinaryPointer = &backing
	nativePointerBits = uintptr(unsafe.Pointer(&backing))
	pointer = &backing
}

//go:noinline
func ReadOrdinaryGlobal() uintptr {
	return uintptr(unsafe.Pointer(ordinaryPointer))
}

//go:noinline
func ReadNativeTLS() uintptr {
	return nativePointerBits
}

//go:noinline
func ReadGLSPackage() uintptr {
	return uintptr(unsafe.Pointer(pointer))
}
