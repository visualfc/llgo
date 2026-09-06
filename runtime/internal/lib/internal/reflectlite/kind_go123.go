//go:build go1.23

package reflectlite

// Go 1.23 narrowed internal/abi.Kind to one byte.
type kindRepr = uint8
