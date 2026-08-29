//go:build !windows

package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/os"
)

func main() {
	verifyGetcwd()

	filenameText := temporaryFilename()
	var filenameBuffer [64]c.Char
	filename := writeCString(filenameBuffer[:], filenameText)
	defer os.Remove(filename)
	data := c.Str("Hello, os!")
	var buffer [20]c.Char

	// Open a file, O_CREAT|O_WRONLY|O_TRUNC means create, write only, or clear the file
	fd := os.Open(filename, os.O_CREAT|os.O_WRONLY|os.O_TRUNC, 0644)
	if fd == -1 {
		panic(fileError("open for write failed", filenameText))
	}

	// Writing data to a file
	bytesWritten := os.Write(fd, c.Pointer(data), c.Strlen(data))
	if bytesWritten != int(c.Strlen(data)) {
		os.Close(fd)
		panic("write failed")
	}

	// Get file status flags
	flags := os.Fcntl(fd, os.F_GETFL)
	if flags == -1 {
		os.Close(fd)
		panic("F_GETFL failed")
	}

	// Set the file status flag to non-blocking mode
	if os.Fcntl(fd, os.F_SETFL, flags|os.O_NONBLOCK) == -1 {
		os.Close(fd)
		panic("F_SETFL failed")
	}
	if updated := os.Fcntl(fd, os.F_GETFL); updated == -1 || updated&os.O_NONBLOCK == 0 {
		os.Close(fd)
		panic("F_SETFL did not set O_NONBLOCK")
	}
	if os.Close(fd) != 0 {
		panic("close after write failed")
	}

	// Reopen the file, O_RDONLY means read-only
	fd = os.Open(filename, os.O_RDONLY)
	if fd == -1 {
		panic(fileError("open for read failed", filenameText))
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
