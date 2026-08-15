//go:build windows && arm64

package runtime

import "unsafe"

// windowsFaultContext mirrors the control/integer prefix of ARM64_NT_CONTEXT.
type windowsFaultContext struct {
	ContextFlags uint32
	Cpsr         uint32
	X            [31]uint64
	SP           uint64
	PC           uint64
}

func windowsFaultPCFP(raw unsafe.Pointer) (pc, fp uintptr) {
	context := (*windowsFaultContext)(raw)
	return uintptr(context.PC), uintptr(context.X[29])
}
