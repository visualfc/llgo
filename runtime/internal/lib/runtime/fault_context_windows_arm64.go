//go:build windows && arm64

package runtime

import "unsafe"

type windowsNeon128 struct {
	Low  uint64
	High int64
}

// windowsFaultContext mirrors Go's internal/runtime/syscall/windows.Context,
// which in turn follows ARM64_NT_CONTEXT. RtlVirtualUnwind may update the full
// record, so this must not be shortened to the integer prefix.
type windowsFaultContext struct {
	ContextFlags uint32
	Cpsr         uint32
	X            [31]uint64
	XSp          uint64
	Pc           uint64
	V            [32]windowsNeon128
	Fpcr         uint32
	Fpsr         uint32
	Bcr          [8]uint32
	Bvr          [8]uint64
	Wcr          [2]uint32
	Wvr          [2]uint64
}

const (
	windowsFaultContextSize     = unsafe.Sizeof(windowsFaultContext{})
	windowsFaultContextSPOffset = unsafe.Offsetof(windowsFaultContext{}.XSp)
	windowsFaultContextPCOffset = unsafe.Offsetof(windowsFaultContext{}.Pc)
)

// Keep the locally declared record ABI-identical to ARM64_NT_CONTEXT.
var (
	_ [912 - windowsFaultContextSize]byte
	_ [windowsFaultContextSize - 912]byte
	_ [256 - windowsFaultContextSPOffset]byte
	_ [windowsFaultContextSPOffset - 256]byte
	_ [264 - windowsFaultContextPCOffset]byte
	_ [windowsFaultContextPCOffset - 264]byte
)

func (context *windowsFaultContext) pc() uintptr { return uintptr(context.Pc) }
func (context *windowsFaultContext) sp() uintptr { return uintptr(context.XSp) }
func (context *windowsFaultContext) lr() uintptr { return uintptr(context.X[30]) }
func (context *windowsFaultContext) setPC(pc uintptr) {
	context.Pc = uint64(pc)
}
func (context *windowsFaultContext) setSP(sp uintptr) {
	context.XSp = uint64(sp)
}
func (context *windowsFaultContext) setLR(lr uintptr) {
	context.X[30] = uint64(lr)
}

func windowsFaultPCFP(raw unsafe.Pointer) (pc, fp uintptr) {
	context := (*windowsFaultContext)(raw)
	return context.pc(), uintptr(context.X[29])
}
