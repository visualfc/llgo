//go:build 386

package main

import (
	"unsafe"
	_ "unsafe"
)

//go:linkname getsp llgo.stackSave
func getsp() unsafe.Pointer

//go:linkname asmFull llgo.asm
func asmFull(instruction string, regs map[string]any) uintptr { return 0 }

func main() {
	sp := asmFull("movl %esp, {}", nil)

	if sp != uintptr(getsp()) {
		panic("invalid stack pointer")
	}
}
