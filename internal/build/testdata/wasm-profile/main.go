package main

import "unsafe"

const LLGoFiles = "_wrap/profile.c"

//go:linkname cPointerSize C.llgo_wasm_profile_pointer_size
func cPointerSize() uintptr

//go:linkname cLongSize C.llgo_wasm_profile_long_size
func cLongSize() uintptr

func main() {
	if got := unsafe.Sizeof(uintptr(0)); got != expectedWordSize {
		panic("unexpected Go word size")
	}
	if got := cPointerSize(); got != expectedWordSize {
		panic("Go and C pointer sizes disagree")
	}
	if got := cLongSize(); got != expectedCLongSize {
		panic("unexpected C long size")
	}
	println("wasm ABI profile ok")
}
