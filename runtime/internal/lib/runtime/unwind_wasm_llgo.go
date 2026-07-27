//go:build wasm

package runtime

func fpCallers(skip int, pc []uintptr) int {
	_, _ = skip, pc
	return 0
}

func fpUnwindAvailable() bool {
	return false
}

// callersWithPanicSplice is unreachable while fpUnwindAvailable is false,
// but it must remain in the selected source set for extern.go to type-check.
func callersWithPanicSplice(_ int, _ []uintptr) int {
	return 0
}
