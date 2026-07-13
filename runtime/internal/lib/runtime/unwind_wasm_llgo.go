//go:build wasm

package runtime

func fpCallers(skip int, pc []uintptr) int {
	_, _ = skip, pc
	return 0
}

func fpUnwindAvailable() bool {
	return false
}
