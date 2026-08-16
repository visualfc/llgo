//go:build windows && amd64

package runtime

import "unsafe"

type windowsM128 struct {
	Low  uint64
	High int64
}

// windowsFaultContext mirrors Go's internal/runtime/syscall/windows.Context,
// which in turn follows the Windows AMD64 CONTEXT ABI. RtlVirtualUnwind may
// update the full record, so this must not be shortened to the integer prefix.
type windowsFaultContext struct {
	P1Home               uint64
	P2Home               uint64
	P3Home               uint64
	P4Home               uint64
	P5Home               uint64
	P6Home               uint64
	ContextFlags         uint32
	MxCsr                uint32
	SegCs                uint16
	SegDs                uint16
	SegEs                uint16
	SegFs                uint16
	SegGs                uint16
	SegSs                uint16
	EFlags               uint32
	Dr0                  uint64
	Dr1                  uint64
	Dr2                  uint64
	Dr3                  uint64
	Dr6                  uint64
	Dr7                  uint64
	Rax                  uint64
	Rcx                  uint64
	Rdx                  uint64
	Rbx                  uint64
	Rsp                  uint64
	Rbp                  uint64
	Rsi                  uint64
	Rdi                  uint64
	R8                   uint64
	R9                   uint64
	R10                  uint64
	R11                  uint64
	R12                  uint64
	R13                  uint64
	R14                  uint64
	R15                  uint64
	Rip                  uint64
	_                    [512]byte
	VectorRegister       [26]windowsM128
	VectorControl        uint64
	DebugControl         uint64
	LastBranchToRip      uint64
	LastBranchFromRip    uint64
	LastExceptionToRip   uint64
	LastExceptionFromRip uint64
}

const (
	windowsFaultContextSize     = unsafe.Sizeof(windowsFaultContext{})
	windowsFaultContextSPOffset = unsafe.Offsetof(windowsFaultContext{}.Rsp)
	windowsFaultContextFPOffset = unsafe.Offsetof(windowsFaultContext{}.Rbp)
	windowsFaultContextPCOffset = unsafe.Offsetof(windowsFaultContext{}.Rip)
)

// Keep the locally declared record ABI-identical to Windows CONTEXT. Importing
// Go's internal Windows package is prohibited from this external module.
var (
	_ [1232 - windowsFaultContextSize]byte
	_ [windowsFaultContextSize - 1232]byte
	_ [152 - windowsFaultContextSPOffset]byte
	_ [windowsFaultContextSPOffset - 152]byte
	_ [160 - windowsFaultContextFPOffset]byte
	_ [windowsFaultContextFPOffset - 160]byte
	_ [248 - windowsFaultContextPCOffset]byte
	_ [windowsFaultContextPCOffset - 248]byte
)

func (context *windowsFaultContext) pc() uintptr { return uintptr(context.Rip) }
func (context *windowsFaultContext) sp() uintptr { return uintptr(context.Rsp) }
func (context *windowsFaultContext) lr() uintptr { return 0 }
func (context *windowsFaultContext) setPC(pc uintptr) {
	context.Rip = uint64(pc)
}
func (context *windowsFaultContext) setSP(sp uintptr) {
	context.Rsp = uint64(sp)
}
func (context *windowsFaultContext) setLR(uintptr) {}

func windowsFaultPCFP(raw unsafe.Pointer) (pc, fp uintptr) {
	context := (*windowsFaultContext)(raw)
	return context.pc(), uintptr(context.Rbp)
}
