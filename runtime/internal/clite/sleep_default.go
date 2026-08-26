//go:build !windows

package c

import _ "unsafe"

//go:linkname Usleep C.usleep
func Usleep(useconds Uint) Int
