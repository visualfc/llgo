//go:build llgo.wasm.emscripten.memory64

package main

import "unsafe"

func checkWasmModel() {
	if unsafe.Sizeof(uintptr(0)) != 8 {
		panic("-target emscripten-memory64 must use 64-bit words")
	}
	if cLongSize() != 8 {
		panic("-target emscripten-memory64 must use the LP64 C data model")
	}
}
