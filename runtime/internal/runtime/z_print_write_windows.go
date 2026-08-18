//go:build windows

package runtime

import "unsafe"

const platformLLGoFiles = "; _wrap/print_windows.c"

//go:linkname c_printByte C.llgo_print_byte
func c_printByte(v byte)

//go:linkname c_printWrite C.llgo_print_write
func c_printWrite(data unsafe.Pointer, size uintptr)

func PrintByte(v byte) {
	c_printByte(v)
}

func printWrite(data unsafe.Pointer, size uintptr) {
	c_printWrite(data, size)
}
