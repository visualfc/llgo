//go:build !llgo.wasm.emscripten.memory64

package main

const (
	expectedWordSize  = 4
	expectedCLongSize = 4
)
