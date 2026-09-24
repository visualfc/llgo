//go:build llgo && wasip1 && wasm && llgo.wasm.gc.linear && llgo.wasi_threads

package gcroot

// The compiler names these slots directly. Each WASI pthread must keep its
// own root chain, including the main thread and threads entering from C.
// Address-bearing slots use uintptr so the root-chain boundary helpers do not
// acquire compiler root frames while changing the chain itself.
//
//llgointernal:tls
var (
	currentRootChain uintptr
	sjljReplaying    bool
	activeContext    uintptr
	rebuilding       bool
)
