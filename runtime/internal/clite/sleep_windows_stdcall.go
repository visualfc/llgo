//go:build windows

package c

import _ "unsafe"

// Sleep is stdcall on Windows/386. Windows/amd64 and Windows/arm64 use their
// unified native C ABI for declarations in the stdcall namespace.
//
//go:linkname winSleep stdcall.Sleep
func winSleep(milliseconds Uint)
