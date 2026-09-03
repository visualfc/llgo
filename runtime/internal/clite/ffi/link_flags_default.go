//go:build !wasm

package ffi

const (
	LLGoPackage = "link: $(pkg-config --libs libffi); -lffi"
	LLGoFiles   = "$(pkg-config --cflags libffi): _wrap/libffi.c"
)
