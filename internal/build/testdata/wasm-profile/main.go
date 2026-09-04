package main

import "unsafe"

const LLGoFiles = "_wrap/profile.c"

//go:linkname cPointerSize C.llgo_wasm_profile_pointer_size
func cPointerSize() uintptr

//go:linkname cLongSize C.llgo_wasm_profile_long_size
func cLongSize() uintptr

func keepDynamic(fn func()) func() {
	return fn
}

func plainRecover() {
	if got := recover(); got != 42 {
		panic("plain function value did not recover")
	}
}

func checkClosureABI() {
	base := 41
	add1 := func() int { return base + 1 }
	if got := add1(); got != 42 {
		panic("captured closure call failed")
	}
	func() {
		defer keepDynamic(plainRecover)()
		panic(42)
	}()
}

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
	checkClosureABI()
	println("wasm ABI profile ok")
}
