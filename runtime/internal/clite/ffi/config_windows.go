//go:build windows

package ffi

const (
	// Avoid clang's MSVC-style -lffi lookup and pass the pinned MSYS2
	// package's ABI-compatible COFF import archive explicitly.
	LLGoPackage = "link: -Wl,$(pkg-config --variable=libdir libffi)/libffi.dll.a"
	LLGoFiles   = "$(pkg-config --cflags libffi): _wrap/libffi.c"
)
