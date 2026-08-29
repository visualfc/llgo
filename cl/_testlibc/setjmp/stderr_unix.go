//go:build !darwin && !windows

package main

import "unsafe"

//go:linkname stderr stderr
var stderr unsafe.Pointer
