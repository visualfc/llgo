package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
)

//go:linkname cSqrt C.sqrt
func cSqrt(x float64) float64

func concat(parts ...string) (result string) {
	for _, part := range parts {
		result += part
	}
	return
}

// Canonical C-FFI example. It combines the former hello, concat, and focused
// embedded libc-memory paths; c/helloc remains the workflow-owned WASI case.
func main() {
	verifyCMemory()
	message := concat("Hello", ", ", "C")
	cMessage := c.AllocaCStr(message)
	if c.GoString(cMessage) != message || c.Strlen(cMessage) != uintptr(len(message)) {
		panic("C string round trip")
	}
	if got := cSqrt(2); got < 1.41421356237 || got > 1.41421356238 {
		panic("C linkname sqrt")
	}
	c.Printf(c.Str("%s; int=%zu uintptr=%zu\n"), cMessage, unsafe.Sizeof(int(0)), unsafe.Sizeof(uintptr(0)))
	printStderr(message)
}
