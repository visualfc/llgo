package main

import (
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
