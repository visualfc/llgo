//go:build !windows

package runtime

import _ "unsafe"

//go:linkname traceClockNow runtime.traceClockNow
func traceClockNow() uint64 { return 0 }
