//go:build windows

package ffi

const (
	// MSVC-targeted clang translates -lffi to ffi.lib, while the pinned
	// MSYS2 package exposes an ABI-compatible COFF import archive.
	LLGoPackage = "link: -Wl,$(pkg-config --variable=libdir libffi)/libffi.dll.a"
	LLGoFiles   = "$(pkg-config --cflags libffi): _wrap/libffi.c"
)
