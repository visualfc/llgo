//go:build windows

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

const (
	windowsSIGSEGV                = 11
	windowsMinPanicOnFaultAddress = 0x1000
)

// WindowsFaultSnapshot, when set by the public runtime package, records the
// fault context before the core converts the exception into a Go panic.
var WindowsFaultSnapshot func(unsafe.Pointer)

//go:linkname installWindowsFaultHandler C.llgo_install_windows_fault_handler
func installWindowsFaultHandler(cb func(unsafe.Pointer, int32, uintptr)) c.Int

//go:linkname windowsFaultCaptureDone C.llgo_windows_fault_capture_done
func windowsFaultCaptureDone()

func init() {
	if installWindowsFaultHandler(onWindowsFault) == 0 {
		panic("runtime: failed to install Windows fault handler")
	}
}

func onWindowsFault(context unsafe.Pointer, signal int32, address uintptr) {
	if signal == windowsSIGSEGV && address >= windowsMinPanicOnFaultAddress && !PanicOnFault() {
		return
	}
	if WindowsFaultSnapshot != nil {
		WindowsFaultSnapshot(context)
	}

	// The panic path does not return through the vectored handler, so release
	// its recursion guard before the non-local jump begins.
	windowsFaultCaptureDone()
	if signal == windowsSIGSEGV && address >= windowsMinPanicOnFaultAddress {
		PanicSignalAddr(address)
	}
	PanicSignal(int(signal))
}
