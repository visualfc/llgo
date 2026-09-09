//go:build wasm && (!js || llgo.wasm.emscripten.memory64)

package ffi

// libffi's WebAssembly backend currently supports the wasm32 Emscripten ABI
// but not WASI or wasm64. Emscripten Memory64 (llgo.wasm.emscripten.memory64)
// lowers to wasm64-unknown-emscripten, so it shares this stub with WASI.
// Keep the low-level boundary linkable; unsupported calls fail explicitly
// instead of accidentally linking the host machine's libffi archive.
const LLGoFiles = "_wrap/libffi_wasm_stub.c"
