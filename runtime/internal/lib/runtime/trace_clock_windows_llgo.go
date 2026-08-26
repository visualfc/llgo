//go:build windows

package runtime

import _ "unsafe"

const windowsTraceTimeDiv = 64

//go:linkname traceClockNow runtime.traceClockNow
func traceClockNow() uint64 { return uint64(runtimeNano()) / windowsTraceTimeDiv }
