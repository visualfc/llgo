package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/os"
)

// Windows has no fcntl API. Exercise the equivalent Universal CRT descriptor
// operations while the Unix source continues to cover F_GETFL/F_SETFL.
func main() {
	filename := c.Str("testfile.txt")
	data := c.Str("Hello, os!")
	defer os.Remove(filename)

	fd := os.Open(filename, os.O_CREAT|os.O_WRONLY|os.O_TRUNC|os.O_BINARY, 0o644)
	if fd == -1 {
		panic("open for write failed")
	}
	if n := os.Write(fd, c.Pointer(data), c.Strlen(data)); n != int(c.Strlen(data)) {
		os.Close(fd)
		panic("write failed")
	}
	if os.Close(fd) != 0 {
		panic("close after write failed")
	}

	fd = os.Open(filename, os.O_RDONLY|os.O_BINARY)
	if fd == -1 {
		panic("open for read failed")
	}
	var buffer [20]c.Char
	n := os.Read(fd, c.Pointer(unsafe.SliceData(buffer[:])), uintptr(len(buffer)-1))
	if n < 0 {
		os.Close(fd)
		panic("read failed")
	}
	buffer[n] = 0
	if os.Close(fd) != 0 {
		panic("close after read failed")
	}
	if got := c.GoString(&buffer[0]); got != "Hello, os!" {
		panic("unexpected file contents: " + got)
	}
	c.Printf(c.Str("Read %d bytes: %s\n"), n, &buffer[0])
}
