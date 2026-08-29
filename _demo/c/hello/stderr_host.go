//go:build !baremetal

package main

import "github.com/goplus/lib/c"

func printStderr(message string) {
	c.Fprintf(c.Stderr, c.Str("C stderr: %s\n"), c.AllocaCStr(message))
}
