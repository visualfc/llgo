//go:build !windows

package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/os"
	"github.com/goplus/lib/c/syscall"
)

func main() {
	verifyGetcwd()

	filename := c.Str("testfile.txt")
	defer os.Remove(filename)
	data := c.Str("Hello, os!")
	var buffer [20]c.Char

	// Open a file, O_CREAT|O_WRONLY|O_TRUNC means create, write only, or clear the file
	// open and fcntl consume the host libc's numeric flags. Use lib/c's
	// target-specific syscall constants rather than non-Windows compatibility
	// values that cannot represent both Darwin and Linux.
	openFlags := c.Int(syscall.O_CREAT | syscall.O_WRONLY | syscall.O_TRUNC)
	fd := os.Open(filename, openFlags, c.Int(0o644))
	if fd == -1 {
		panic("open for write failed")
	}

	// Writing data to a file
	bytesWritten := os.Write(fd, c.Pointer(data), c.Strlen(data))
	if bytesWritten != int(c.Strlen(data)) {
		os.Close(fd)
		panic("write failed")
	}

	// Get file status flags
	flags := os.Fcntl(fd, syscall.F_GETFL)
	if flags == -1 {
		os.Close(fd)
		panic("F_GETFL failed")
	}

	// Set the file status flag to non-blocking mode
	nonblock := c.Int(syscall.O_NONBLOCK)
	if os.Fcntl(fd, syscall.F_SETFL, flags|nonblock) == -1 {
		os.Close(fd)
		panic("F_SETFL failed")
	}
	if updated := os.Fcntl(fd, syscall.F_GETFL); updated == -1 || updated&nonblock == 0 {
		os.Close(fd)
		panic("F_SETFL did not set O_NONBLOCK")
	}
	if os.Close(fd) != 0 {
		panic("close after write failed")
	}

	// Reopen the file, O_RDONLY means read-only
	fd = os.Open(filename, syscall.O_RDONLY)
	if fd == -1 {
		panic("open for read failed")
	}

	// Reading data from a file
	// &buffer[:][0]
	// unsafe.SliceData(buffer[:])
	bytesRead := os.Read(fd, c.Pointer(unsafe.SliceData(buffer[:])), unsafe.Sizeof(buffer)-1)
	if bytesRead != int(c.Strlen(data)) {
		os.Close(fd)
		panic("read failed")
	}

	// Ensure that the buffer is null-terminated
	buffer[bytesRead] = c.Char(0)
	if got := c.GoString(&buffer[0]); got != "Hello, os!" {
		os.Close(fd)
		panic("unexpected file contents: " + got)
	}
	if os.Close(fd) != 0 {
		panic("close after read failed")
	}
	c.Printf(c.Str("Read %ld bytes: %s\n"), bytesRead, &buffer[0])
}
