//go:build windows

package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/os"
)

func getcwd(buffer c.Pointer, size int) *c.Char {
	return os.Getcwd(buffer, c.Int(size))
}
