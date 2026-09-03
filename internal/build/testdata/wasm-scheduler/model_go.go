//go:build !llgo.wasm.emscripten.memory64

package main

import (
	"unsafe"
)

func checkWasmModel() {
	if unsafe.Sizeof(uintptr(0)) != 4 {
		panic("raw and wasm32 C profiles must use 32-bit words")
	}
	if cLongSize() != 4 {
		panic("raw and wasm32 C profiles must use the wasm32 C data model")
	}
}
