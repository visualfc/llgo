package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
)

// Canonical C-FFI example. It combines the former hello, concat, and focused
// embedded libc-memory paths; c/helloc remains the workflow-owned WASI case.
func main() {
	verifyCMemory()
	message := "Hello" + ", " + "C"
	cMessage := c.AllocaCStr(message)
	if c.GoString(cMessage) != message || c.Strlen(cMessage) != uintptr(len(message)) {
		panic("C string round trip")
	}
	c.Printf(c.Str("%s; int=%zu uintptr=%zu\n"), cMessage, unsafe.Sizeof(int(0)), unsafe.Sizeof(uintptr(0)))
	printStderr(message)
}
