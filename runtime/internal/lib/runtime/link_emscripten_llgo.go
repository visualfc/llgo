//go:build js && wasm && llgo.wasm.emscripten

package runtime

import (
	"unsafe"
	_ "unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

//go:linkname cGetentropy C.getentropy
//go:noescape
func cGetentropy(buffer unsafe.Pointer, length uintptr) c.Int

// crypto/internal/sysrand uses the official Go js/wasm host symbol. The
// explicit Emscripten profiles keep that standard-library call site and bridge
// only its host boundary to Emscripten libc.
//
//go:linkname runtimeGetRandomData runtime.getRandomData
func runtimeGetRandomData(buffer []byte) {
	for len(buffer) > 0 {
		n := len(buffer)
		if n > 256 {
			n = 256
		}
		if cGetentropy(unsafe.Pointer(&buffer[0]), uintptr(n)) != 0 {
			fatal("crypto/rand: failed to read random data")
		}
		buffer = buffer[n:]
	}
}
