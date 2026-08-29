package main

import (
	"strconv"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/os"
)

func verifyGetcwd() {
	buffer := c.Alloca(os.PATH_MAX)
	wd := getcwd(buffer, os.PATH_MAX)
	if wd == nil || c.GoString(wd) == "" {
		panic("getcwd")
	}
}

func temporaryFilename() string {
	// The demo runner executes each case from its writable source directory.
	// A PID suffix isolates concurrent cases without adding a Go os.TempDir
	// dependency to this C descriptor test.
	return "llgo-fcntl-" + strconv.Itoa(int(os.Getpid())) + ".tmp"
}
