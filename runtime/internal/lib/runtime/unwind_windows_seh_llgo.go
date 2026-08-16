//go:build windows && (amd64 || arm64)

package runtime

import "unsafe"

// Win64 frame registers are addressing bases, not conventional linked frame
// records: UNWIND_INFO may establish RBP/X29 at a non-zero offset into the
// frame. Use the platform unwinder, which consumes the same .pdata/.xdata
// records that the compiler emits for exception handling.

//go:linkname c_windowsCaptureContext C.llgo_windows_capture_context
func c_windowsCaptureContext(context *windowsFaultContext, pcOffset uintptr) unsafe.Pointer

//go:linkname c_windowsLookupFunctionEntry C.llgo_windows_lookup_function_entry
func c_windowsLookupFunctionEntry(pc uintptr, imageBase *uintptr) unsafe.Pointer

//go:linkname c_windowsVirtualUnwind C.llgo_windows_virtual_unwind
func c_windowsVirtualUnwind(imageBase, pc uintptr, functionEntry unsafe.Pointer, context *windowsFaultContext, establisherFrame *uintptr) unsafe.Pointer

func windowsUnwindOne(context *windowsFaultContext) bool {
	pc := context.pc()
	if pc < minLegalPC {
		return false
	}
	var imageBase uintptr
	entry := c_windowsLookupFunctionEntry(pc, &imageBase)
	if entry != nil {
		var frame uintptr
		c_windowsVirtualUnwind(imageBase, pc, entry, context, &frame)
		return context.pc() >= minLegalPC && context.pc() != pc
	}

	// Leaf functions have no RUNTIME_FUNCTION entry. Simulate their return:
	// arm64 uses LR; amd64 takes the return pc from the current stack pointer.
	if lr := context.lr(); lr >= minLegalPC {
		context.setPC(lr)
		context.setLR(0)
		return true
	}
	sp := context.sp()
	wordSize := unsafe.Sizeof(uintptr(0))
	if sp&(wordSize-1) != 0 || !memReadable(sp) {
		return false
	}
	ret := *(*uintptr)(unsafe.Pointer(sp))
	if ret < minLegalPC {
		return false
	}
	context.setSP(sp + wordSize)
	context.setPC(ret)
	return true
}

func windowsContextCallers(context *windowsFaultContext, skip int, pc []uintptr) int {
	n := 0
	for i := 0; n < len(pc) && i < maxPanicSpliceFrames; i++ {
		if !windowsUnwindOne(context) {
			break
		}
		ret := context.pc()
		if skip > 0 {
			skip--
			continue
		}
		if !prebuiltTextContains(ret) {
			break
		}
		pc[n] = ret
		n++
	}
	return n
}

//go:noinline
func platformCallers(_ uintptr, skip int, pc []uintptr) int {
	var context windowsFaultContext
	if c_windowsCaptureContext(&context, windowsFaultContextPCOffset) == nil {
		return 0
	}
	// The capture wrapper already unwound itself. Drop platformCallers; the
	// return from fpCallers, matching framePointerCallers' first entry.
	return windowsContextCallers(&context, skip+1, pc)
}

func platformFaultCallers(raw unsafe.Pointer, _ uintptr, pc []uintptr) int {
	// Keep the OS-owned exception record intact. Windows still owns it while
	// the vectored handler is active, even though LLGo leaves through its
	// non-local panic path rather than resuming the faulting instruction.
	context := *(*windowsFaultContext)(raw)
	return windowsContextCallers(&context, 0, pc)
}

//go:noinline
func recoverFrameMarks() (uintptr, uintptr) {
	var context windowsFaultContext
	if c_windowsCaptureContext(&context, windowsFaultContextPCOffset) == nil {
		return 0, 0
	}
	// The capture wrapper already unwound itself. Walk recoverFrameMarks ->
	// recoverMark -> Recover -> the deferred function, then record both its
	// stack identity and function entry.
	for i := 0; i < 3; i++ {
		if !windowsUnwindOne(&context) {
			return 0, 0
		}
	}
	entry := frameSymbol(context.pc() - 1).entry
	if entry == 0 {
		return 0, 0
	}
	return context.sp(), entry
}

//go:noinline
func recoverFrameLive(stack, entry uintptr) bool {
	if stack == 0 || entry == 0 {
		return false
	}
	var context windowsFaultContext
	if c_windowsCaptureContext(&context, windowsFaultContextPCOffset) == nil {
		return false
	}
	for i := 0; i < maxPanicSpliceFrames; i++ {
		if !windowsUnwindOne(&context) {
			break
		}
		if context.sp() == stack && frameSymbol(context.pc()-1).entry == entry {
			return true
		}
	}
	return false
}
