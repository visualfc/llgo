//go:build windows

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

const (
	windowsExceptionAccessViolation uint32 = 0xc0000005
	windowsExceptionInPageError     uint32 = 0xc0000006
	windowsExceptionIntDivideByZero uint32 = 0xc0000094
	windowsExceptionIntOverflow     uint32 = 0xc0000095
	windowsSIGFPE                          = 8
	windowsSIGSEGV                         = 11
	windowsMinPanicOnFaultAddress          = 0x1000
)

// WindowsFaultSnapshot, when set by the public runtime package, first reports
// whether the fault PC belongs to Go text and then records the fault context.
// A false result leaves exceptions raised by native code to Windows' handler
// chain, matching the Go runtime's isgoexception check.
var WindowsFaultSnapshot func(unsafe.Pointer) bool

//go:linkname installWindowsFaultHandler C.llgo_install_windows_fault_handler
func installWindowsFaultHandler(cb func(unsafe.Pointer, uint32, uintptr)) c.Int

//go:linkname windowsFaultCaptureDone C.llgo_windows_fault_capture_done
func windowsFaultCaptureDone()

func init() {
	if installWindowsFaultHandler(onWindowsFault) == 0 {
		panic("runtime: failed to install Windows fault handler")
	}
}

func onWindowsFault(context unsafe.Pointer, code uint32, address uintptr) {
	// The vectored handler is process-wide and may observe a fault on a native
	// thread that never entered Go. Do not manufacture a G from exception
	// context: only faults on a thread already executing Go can become Go
	// panics. Foreign faults must continue through Windows' handler chain.
	// currentG is a uintptr in the LLGo TLS build and a *g in host and
	// baremetal builds. Normalize both representations without calling getg,
	// which would incorrectly create a G for a foreign faulting thread.
	if (*g)(unsafe.Pointer(currentG)) == nil {
		return
	}
	memoryFault := code == windowsExceptionAccessViolation || code == windowsExceptionInPageError
	if memoryFault && address >= windowsMinPanicOnFaultAddress && !PanicOnFault() {
		return
	}
	if WindowsFaultSnapshot != nil && !WindowsFaultSnapshot(context) {
		return
	}

	// The panic path does not return through the vectored handler, so release
	// its recursion guard before the non-local jump begins.
	windowsFaultCaptureDone()
	panicWindowsException(code, address)
}

func panicWindowsException(code uint32, address uintptr) {
	memoryFault := code == windowsExceptionAccessViolation || code == windowsExceptionInPageError
	if memoryFault && address >= windowsMinPanicOnFaultAddress {
		PanicSignalAddr(address)
	}
	switch code {
	case windowsExceptionAccessViolation, windowsExceptionInPageError:
		PanicSignal(windowsSIGSEGV)
	case windowsExceptionIntDivideByZero:
		PanicSignal(windowsSIGFPE)
	case windowsExceptionIntOverflow:
		PanicErrorString("integer overflow")
	}
}
