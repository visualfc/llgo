//go:build baremetal

package main

// Bare-metal C libraries used by the embedded smoke tests do not expose
// stderr. The host build keeps the c.Fprintf/c.Stderr coverage.
func printStderr(string) {}
