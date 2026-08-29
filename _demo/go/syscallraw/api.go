package main

import (
	"syscall"
	"unsafe"

	"github.com/goplus/lib/c"
)

func printSyscallError(prefix string, err error) {
	if err == nil {
		return
	}
	if errno, ok := err.(syscall.Errno); ok {
		c.Printf(c.Str("%s: errno=%d\n"), c.AllocaCStr(prefix), errno)
		return
	}
	c.Printf(c.Str("%s: error\n"), c.AllocaCStr(prefix))
}

// Preserve the portable syscall wrapper calls next to the raw syscall owner.
// On Windows /etc/hosts is expected to take the original error path.
func verifySyscallAPI() {
	c.Printf(c.Str("pid=%d\n"), syscall.Getpid())
	if workingDirectory, err := syscall.Getwd(); err != nil {
		printSyscallError("getwd", err)
	} else {
		c.Printf(c.Str("cwd=%s\n"), c.AllocaCStr(workingDirectory))
	}

	fd, err := syscall.Open("/etc/hosts", 0, 0)
	if err != nil {
		printSyscallError("open /etc/hosts", err)
		return
	}
	defer syscall.Close(fd)

	var buffer [128]byte
	count, err := syscall.Read(fd, buffer[:])
	if err != nil {
		printSyscallError("read /etc/hosts", err)
		return
	}
	c.Printf(c.Str("read=%d\n"), count)
	if count > 0 {
		c.Printf(c.Str("head: %.*s\n"), count, (*c.Char)(unsafe.Pointer(&buffer[0])))
	}
}
