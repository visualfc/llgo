//go:build !windows

package runtime

import _ "unsafe"

//go:linkname fastrand C.rand
func fastrand() uint32
