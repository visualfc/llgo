//go:build llgo

// Package dep1 provides utility functions for cache testing.
package dep1

// Add returns the sum of two integers.
func Add(a, b int) int {
	return a + b
}

// Msg returns a greeting message.
func Msg() string {
	return "dep1"
}

// Recover stores the panic value seen by a directly deferred dependency call.
func Recover(dst *any) {
	*dst = recover()
}
