//go:build wasm && js && llgo.wasm.emscripten.memory64

package ffi

// The wasm64 libffi archive is searched via the Emscripten -L path in
// internal/crosscompile. Closures require ALLOW_TABLE_GROWTH, which is
// enabled on the same linker command line.
const (
	LLGoPackage = "link: -lffi"
	LLGoFiles   = "_wrap/libffi_wasm64_js.c"
)
