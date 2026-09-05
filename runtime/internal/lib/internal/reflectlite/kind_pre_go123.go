//go:build !go1.23

package reflectlite

// Go 1.20-1.22 use a word-sized Kind.
type kindRepr = uint
