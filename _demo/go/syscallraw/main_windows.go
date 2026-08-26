package main

import (
	"runtime"
	"syscall"
	"unsafe"
)

func main() {
	msg := []byte("Hello from Syscall!\n")
	writeFile := syscall.NewLazyDLL("kernel32.dll").NewProc("WriteFile")
	var written uint32
	r1, _, callErr := syscall.SyscallN(
		writeFile.Addr(),
		uintptr(syscall.Stdout),
		uintptr(unsafe.Pointer(&msg[0])),
		uintptr(len(msg)),
		uintptr(unsafe.Pointer(&written)),
		0,
	)
	runtime.KeepAlive(msg)
	if r1 == 0 {
		panic(callErr)
	}
	if written != uint32(len(msg)) {
		panic("short WriteFile")
	}
}
