//go:build windows && 386

package c

import _ "unsafe"

// Sleep uses stdcall on 32-bit Windows, while an unannotated Go declaration
// uses cdecl. Keep the calling-convention adapter limited to 386 so 64-bit
// Windows does not pay for an unnecessary forwarding call without LTO.
const LLGoFiles = "_wrap/sleep_windows.c"

//go:linkname winSleep C.llgo_windows_sleep
func winSleep(milliseconds Uint)
