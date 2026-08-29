package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
)

// Canonical small C-FFI example, also used by the WASM smoke. It combines the
// former hello, helloc and concat programs without pulling in platform APIs.
func main() {
	message := "Hello" + ", " + "C"
	cMessage := c.AllocaCStr(message)
	if c.GoString(cMessage) != message || c.Strlen(cMessage) != uintptr(len(message)) {
		panic("C string round trip")
	}
	c.Printf(c.Str("%s; int=%zu uintptr=%zu\n"), cMessage, unsafe.Sizeof(int(0)), unsafe.Sizeof(uintptr(0)))
	printStderr(message)
}
