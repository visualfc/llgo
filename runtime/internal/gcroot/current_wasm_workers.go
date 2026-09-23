//go:build llgo && js && wasm && llgo.wasm.gc.linear && llgo.wasm.workers

package gcroot

import "unsafe"

// The compiler addresses these fully qualified symbols directly, so their
// declarations and compiler-generated references must both remain native TLS.
//
//llgo:tls
var (
	currentRootChain unsafe.Pointer
	sjljReplaying    bool
	activeContext    *Context
	rebuilding       bool
)
