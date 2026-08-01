//go:build baremetal && !wasm

package runtime

func fpCallers(_ int, _ []uintptr) int {
	return 0
}

func fpUnwindAvailable() bool {
	return false
}

func callersWithPanicSplice(_ int, _ []uintptr) int {
	return 0
}
