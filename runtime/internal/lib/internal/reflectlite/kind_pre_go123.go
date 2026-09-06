//go:build !go1.23

package reflectlite

// Go versions before 1.23 use a word-sized Kind.
type kindRepr = uint
