// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build wasm

package atomic

import "unsafe"

// StorepNoWB performs *ptr = val without a write barrier.
func StorepNoWB(ptr unsafe.Pointer, val unsafe.Pointer) {
	*(*uintptr)(ptr) = uintptr(val)
}
