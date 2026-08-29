//go:build !go1.27

package runtime

func panicNilErrorMessage() string {
	return "panic called with nil argument"
}
