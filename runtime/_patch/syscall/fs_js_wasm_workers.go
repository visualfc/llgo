//go:build js && wasm && llgo.wasm.workers

package syscall

import "syscall/js"

// Node objects are represented by Emscripten emval handles, whose ownership
// is local to one JavaScript worker. Integer flags and the Go file table stay
// process-wide; only the handles are initialized independently per worker.
//
//llgointernal:tls
var (
	jsProcess  = js.Global().Get("process")
	jsPath     = js.Global().Get("path")
	jsFS       = js.Global().Get("fs")
	constants  = jsFS.Get("constants")
	uint8Array = js.Global().Get("Uint8Array")
)
