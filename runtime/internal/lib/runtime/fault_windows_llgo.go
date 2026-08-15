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

var (
	windowsFaultPCs    [64]uintptr
	windowsFaultN      int32
	windowsFaultActive int32
)

func onWindowsFault(context unsafe.Pointer, signal int32) {
	windowsFaultActive = 1
	windowsFaultN = 0
	pc, fp := windowsFaultPCFP(context)
	n := 0
	if pc != 0 {
		// Stored PCs follow runtime.Callers' return-PC convention. Adding one
		// keeps PC-1 on the instruction that raised the exception.
		windowsFaultPCs[0] = pc + 1
		n = 1
	}
	if fpUnwindAvailable() && n < len(windowsFaultPCs) {
		n += windowsFPWalkFrom(fp, windowsFaultPCs[n:])
	}
	windowsFaultN = int32(n)
	rtdebug.StoreFaultPCs(windowsFaultPCs[:n])

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

func clearFaultTraceback() {
	windowsFaultActive = 0
	windowsFaultN = 0
}

func faultTracebackActive() bool { return windowsFaultActive != 0 }

func faultTraceback(skip int) bool {
	if windowsFaultN == 0 || !fpUnwindAvailable() {
		return false
	}
	initRuntimeFuncPCFrames()
	print("goroutine 1 [running]:\n")
	printed := 0
	for i := 0; i < int(windowsFaultN); i++ {
		pc := windowsFaultPCs[i]
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
