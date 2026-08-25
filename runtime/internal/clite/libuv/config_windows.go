//go:build windows

package libuv

const (
	// An MSVC-targeted clang resolves -luv as uv.lib, while MSYS2 packages
	// the ABI-compatible COFF import archive as libuv.dll.a. Name it
	// explicitly; the DLL owns its MinGW implementation dependencies and
	// exposes the same C ABI to MSVC callers.
	LLGoPackage = "link: -Wl,$(pkg-config --variable=libdir libuv)/libuv.dll.a"
	LLGoFiles   = "_wrap/libuv.c"
)
