//go:build windows && 386

package runtime

import "unsafe"

func platformCallers(fp uintptr, skip int, pc []uintptr) int {
	return framePointerCallers(fp, skip, pc)
}

func platformFaultCallers(_ unsafe.Pointer, fp uintptr, pc []uintptr) int {
	return windowsFPWalkFrom(fp, pc)
}

func recoverFrameMarks() (uintptr, uintptr) {
	return framePointerRecoverMarks()
}

func recoverFrameLive(mark1, mark2 uintptr) bool {
	return framePointerRecoverFrameLive(mark1, mark2)
}
