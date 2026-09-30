//go:build llgo && wasip1 && wasm && llgo.wasi_threads && llgo.wasm.gc.linear

package tinygogc

import _ "unsafe"

const segmentedHeap = true

func newHeapSegment(minimum uintptr) (uintptr, uintptr) {
	const segmentSize = uintptr(32 << 20)
	size := segmentSize
	// The nominal arena size includes its metadata. Account for that and
	// alignment at both ends before comparing with the default arena size;
	// even an object slightly smaller than 32 MiB may not fit in that arena.
	extra := minimum/(bytesPerBlock*blocksPerStateByte) + 2*bytesPerBlock
	if minimum > ^uintptr(0)-extra-(wasmPageSize-1) {
		return 0, 0
	}
	if required := alignUp(minimum+extra, wasmPageSize); required > size {
		size = required
	}
	start := gcWasmNewArena(size)
	if start == 0 {
		return 0, 0
	}
	return alignUp(start, bytesPerBlock), alignDown(start+size, bytesPerBlock)
}

//go:linkname gcWasmNewArena C.llgo_gc_new_arena
func gcWasmNewArena(size uintptr) uintptr
