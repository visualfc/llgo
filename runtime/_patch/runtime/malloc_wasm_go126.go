//go:build wasm && go1.26

package runtime

// Go 1.26 reduced wasm heap arenas to 512 KiB. LLGo still needs the heap
// address and allocation limits adjusted for native wasm32 pointers.
const (
	heapAddrBits      = 32
	maxAlloc          = (1 << heapAddrBits) - 1
	logHeapArenaBytes = 19
)
