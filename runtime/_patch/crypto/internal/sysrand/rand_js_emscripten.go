//go:build js && wasm && llgo.wasm.emscripten

//llgo:skip getRandomValues

package sysrand

import _ "unsafe"

// Emscripten does not provide the official Go gojs host module. Preserve the
// standard-library helper contract while resolving it to LLGo's C-ABI bridge.
//
//go:linkname getRandomValues runtime.getRandomData
func getRandomValues(p []byte)
