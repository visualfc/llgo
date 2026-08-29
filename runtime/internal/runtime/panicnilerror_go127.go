//go:build go1.27

package runtime

func panicNilErrorMessage() string {
	return "runtime error: panic called with nil argument"
}
