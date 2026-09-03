//go:build llgo && js && wasm && llgo.wasm.emscripten && llgo.wasm.emscripten.memory64

package emscripten

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

//go:wasmimport wasi_snapshot_preview1 fd_write
func wasiFDWrite(fd uint32, iov, count, written uint64) uint32

func writeStderr(data c.Pointer, size uintptr) {
	iov := wasiCIovec{data: data, size: size}
	var written uintptr
	wasiFDWrite(
		2,
		uint64(uintptr(unsafe.Pointer(&iov))),
		1,
		uint64(uintptr(unsafe.Pointer(&written))),
	)
}
