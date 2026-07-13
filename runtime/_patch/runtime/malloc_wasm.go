//go:build wasm && !go1.26

package runtime

// LLGo uses native wasm32 pointers, while the Go wasm port models uintptr as
// 64 bits. Keep the GC heap geometry in the wasm32 address space.
const (
	heapAddrBits      = 32
	maxAlloc          = (1 << heapAddrBits) - 1
	logHeapArenaBytes = 22
)
