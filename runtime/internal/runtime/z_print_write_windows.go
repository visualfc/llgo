//go:build windows

package runtime

import "unsafe"

const platformLLGoFiles = "; _wrap/print_windows.c"

//go:linkname c_printWrite C.llgo_print_write
func c_printWrite(data unsafe.Pointer, size uintptr)

func printWrite(data unsafe.Pointer, size uintptr) {
	c_printWrite(data, size)
}
