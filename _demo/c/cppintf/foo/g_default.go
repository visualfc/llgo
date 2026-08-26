//go:build !windows

package foo

import _ "unsafe"

//go:linkname G C._Z1gP9ICallback
func G(cb *Callback)
