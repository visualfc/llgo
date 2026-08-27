//go:build windows && (amd64 || arm64)

package c

import _ "unsafe"

// The Windows x64 and ARM64 ABIs have one native calling convention, so the
// system import can be called directly without the 386 stdcall adapter.
//
//go:linkname winSleep C.Sleep
func winSleep(milliseconds Uint)
