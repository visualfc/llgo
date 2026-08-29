package main

import (
	stdos "os"
	"path/filepath"
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
	name := "llgo-fcntl-" + strconv.Itoa(int(os.Getpid())) + ".tmp"
	return filepath.Join(stdos.TempDir(), name)
}
