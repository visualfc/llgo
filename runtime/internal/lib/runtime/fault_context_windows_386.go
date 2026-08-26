//go:build windows && 386

package runtime

import "unsafe"

type windowsFloatingSaveArea struct {
	ControlWord   uint32
	StatusWord    uint32
	TagWord       uint32
	ErrorOffset   uint32
	ErrorSelector uint32
	DataOffset    uint32
	DataSelector  uint32
	RegisterArea  [80]uint8
	Cr0NpxState   uint32
}

// windowsFaultContext mirrors the control/integer prefix of x86 CONTEXT.
type windowsFaultContext struct {
	ContextFlags     uint32
	Dr0              uint32
	Dr1              uint32
	Dr2              uint32
	Dr3              uint32
	Dr6              uint32
	Dr7              uint32
	FloatingSaveArea windowsFloatingSaveArea
	SegGs            uint32
	SegFs            uint32
	SegEs            uint32
	SegDs            uint32
	Edi              uint32
	Esi              uint32
	Ebx              uint32
	Edx              uint32
	Ecx              uint32
	Eax              uint32
	Ebp              uint32
	Eip              uint32
	SegCs            uint32
	EFlags           uint32
	Esp              uint32
}

const (
	windowsFaultContextPrefixSize = unsafe.Sizeof(windowsFaultContext{})
	windowsFaultContextSPOffset   = unsafe.Offsetof(windowsFaultContext{}.Esp)
	windowsFaultContextFPOffset   = unsafe.Offsetof(windowsFaultContext{}.Ebp)
	windowsFaultContextPCOffset   = unsafe.Offsetof(windowsFaultContext{}.Eip)
)

// Keep the locally declared prefix ABI-identical to the control/integer
// prefix of the Windows 386 CONTEXT record.
var (
	_ [200 - windowsFaultContextPrefixSize]byte
	_ [windowsFaultContextPrefixSize - 200]byte
	_ [196 - windowsFaultContextSPOffset]byte
	_ [windowsFaultContextSPOffset - 196]byte
	_ [180 - windowsFaultContextFPOffset]byte
	_ [windowsFaultContextFPOffset - 180]byte
	_ [184 - windowsFaultContextPCOffset]byte
	_ [windowsFaultContextPCOffset - 184]byte
)

func (context *windowsFaultContext) faultCallerPC() uintptr {
	return windowsFaultStackCallerPC(uintptr(context.Esp))
}

func windowsFaultPCFP(raw unsafe.Pointer) (pc, fp uintptr) {
	context := (*windowsFaultContext)(raw)
	return uintptr(context.Eip), uintptr(context.Ebp)
}
