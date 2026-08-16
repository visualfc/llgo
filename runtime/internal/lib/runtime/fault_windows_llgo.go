//go:build windows

/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	rtdebug "github.com/xgo-dev/llgo/runtime/internal/runtime"
)

//go:linkname c_installWindowsFaultHandler C.llgo_install_windows_fault_handler
func c_installWindowsFaultHandler(cb func(unsafe.Pointer, int32)) c.Int

//go:linkname c_windowsFaultCaptureDone C.llgo_windows_fault_capture_done
func c_windowsFaultCaptureDone()

//go:linkname c_windowsFaultPCBuf C.llgo_windows_fault_pcbuf
func c_windowsFaultPCBuf() unsafe.Pointer

//go:linkname c_memReadable C.llgo_mem_readable
func c_memReadable(p unsafe.Pointer) c.Int

func memReadable(addr uintptr) bool {
	return c_memReadable(unsafe.Pointer(addr)) != 0
}

func init() {
	if c_installWindowsFaultHandler(onWindowsFault) == 0 {
		panic("runtime: failed to install Windows fault handler")
	}
}

func onWindowsFault(context unsafe.Pointer, signal int32) {
	// Faults are recoverable and different goroutines run on different host
	// threads, so capture into the handler's thread-local scratch storage.
	// StoreFaultPCs then copies the snapshot into the current G before panic's
	// non-local jump unwinds the handler stack.
	pcs := (*[64]uintptr)(c_windowsFaultPCBuf())
	pc, fp := windowsFaultPCFP(context)
	n := 0
	if pc != 0 {
		// Stored PCs follow runtime.Callers' return-PC convention. Adding one
		// keeps PC-1 on the instruction that raised the exception.
		pcs[0] = pc + 1
		n = 1
	}
	if fpUnwindAvailable() && n < len(pcs) {
		n += platformFaultCallers(context, fp, pcs[n:])
	}
	rtdebug.StoreFaultPCs(pcs[:n])

	// The panic path does not return through the vectored handler, so release
	// its recursion guard before the non-local jump begins.
	c_windowsFaultCaptureDone()
	rtdebug.PanicSignal(int(signal))
}

func windowsFPWalkFrom(fp uintptr, pcs []uintptr) int {
	n := 0
	const maxFrames = 4096
	wordSize := unsafe.Sizeof(uintptr(0))
	for i := 0; fp != 0 && n < len(pcs) && i < maxFrames; i++ {
		if fp&(wordSize-1) != 0 || !memReadable(fp) || !memReadable(fp+wordSize) {
			break
		}
		prev := *(*uintptr)(unsafe.Pointer(fp))
		ret := *(*uintptr)(unsafe.Pointer(fp + wordSize))
		if ret < minLegalPC {
			break
		}
		pcs[n] = ret
		n++
		if prev <= fp || prev-fp > maxFPStride || prev&(wordSize-1) != 0 {
			break
		}
		fp = prev
	}
	return n
}

// Windows fault snapshots live in the current G. Keep a recovered snapshot
// available while its deferred frame can still observe runtime.Callers; the
// next panic replaces it. PanicActive supplies the separate in-flight bit.
func clearFaultTraceback() {}

func faultTracebackActive() bool {
	return rtdebug.PanicPCsAreFault() && rtdebug.PanicActive()
}

func faultTraceback(skip int) bool {
	pcs := rtdebug.PanicPCs()
	if !rtdebug.PanicPCsAreFault() || len(pcs) == 0 || !fpUnwindAvailable() {
		return false
	}
	initRuntimeFuncPCFrames()
	print("goroutine 1 [running]:\n")
	printed := 0
	for _, pc := range pcs {
		if !prebuiltTextContains(pc) {
			break
		}
		sym := frameSymbol(pc - 1)
		name := sym.function
		if name == "" {
			name = unknownFunctionName(pc)
		}
		print(name, "(...)\n\t")
		if sym.file == "" {
			print("pc=0x", string(appendHexUint(nil, pc-1)))
		} else {
			print(sym.file, ":", sym.line)
			if sym.entry != 0 && pc >= sym.entry {
				print(" +0x", string(appendHexUint(nil, pc-sym.entry)))
			}
		}
		print("\n")
		printed++
	}
	return printed > 0
}
