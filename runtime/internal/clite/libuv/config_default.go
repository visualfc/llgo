//go:build !windows

package libuv

const (
	LLGoPackage = "link: $(pkg-config --libs libuv); -luv"
	LLGoFiles   = "$(pkg-config --cflags libuv): _wrap/libuv.c"
)
