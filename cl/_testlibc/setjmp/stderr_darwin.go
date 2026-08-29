//go:build darwin

package main

import "unsafe"

//go:linkname stderr __stderrp
var stderr unsafe.Pointer
