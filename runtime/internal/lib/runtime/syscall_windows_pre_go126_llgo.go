//go:build windows && !go1.26

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

import _ "unsafe"

// Before Go 1.26, package syscall declares these entry points without bodies.
// nargs, rather than the wrapper width, controls how many arguments are
// visible to the callee. Go 1.26 and later provide these wrappers and DLL
// helpers in package syscall and call the common syscall.syscalln entry point.

//go:linkname syscall_Syscall syscall.Syscall
//go:nosplit
func syscall_Syscall(fn, n, a1, a2, a3 uintptr) (r1, r2, err uintptr) {
	return syscall_syscalln(fn, n, a1, a2, a3)
}

//go:linkname syscall_Syscall6 syscall.Syscall6
//go:nosplit
func syscall_Syscall6(fn, n, a1, a2, a3, a4, a5, a6 uintptr) (r1, r2, err uintptr) {
	return syscall_syscalln(fn, n, a1, a2, a3, a4, a5, a6)
}

//go:linkname syscall_Syscall9 syscall.Syscall9
//go:nosplit
func syscall_Syscall9(fn, n, a1, a2, a3, a4, a5, a6, a7, a8, a9 uintptr) (r1, r2, err uintptr) {
	return syscall_syscalln(fn, n, a1, a2, a3, a4, a5, a6, a7, a8, a9)
}

//go:linkname syscall_Syscall12 syscall.Syscall12
//go:nosplit
func syscall_Syscall12(fn, n, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12 uintptr) (r1, r2, err uintptr) {
	return syscall_syscalln(fn, n, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
}

//go:linkname syscall_Syscall15 syscall.Syscall15
//go:nosplit
func syscall_Syscall15(fn, n, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 uintptr) (r1, r2, err uintptr) {
	return syscall_syscalln(fn, n, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
}

//go:linkname syscall_Syscall18 syscall.Syscall18
//go:nosplit
func syscall_Syscall18(fn, n, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18 uintptr) (r1, r2, err uintptr) {
	return syscall_syscalln(fn, n, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
}

//go:linkname syscall_SyscallN syscall.SyscallN
//go:nosplit
func syscall_SyscallN(fn uintptr, args ...uintptr) (r1, r2, err uintptr) {
	return syscall_syscalln(fn, uintptr(len(args)), args...)
}

//go:linkname c_loadLibrary C.llgo_load_library
func c_loadLibrary(filename *uint16, flags uint32, errno *uint32) uintptr

//go:linkname c_getProcAddress C.llgo_get_proc_address
func c_getProcAddress(handle uintptr, name *uint8, errno *uint32) uintptr

//go:linkname syscall_loadlibrary syscall.loadlibrary
func syscall_loadlibrary(filename *uint16) (handle, err uintptr) {
	var errno uint32
	handle = c_loadLibrary(filename, 0, &errno)
	return handle, uintptr(errno)
}

//go:linkname syscall_loadsystemlibrary syscall.loadsystemlibrary
func syscall_loadsystemlibrary(filename *uint16) (handle, err uintptr) {
	const loadLibrarySearchSystem32 = 0x00000800
	var errno uint32
	handle = c_loadLibrary(filename, loadLibrarySearchSystem32, &errno)
	return handle, uintptr(errno)
}

//go:linkname syscall_getprocaddress syscall.getprocaddress
func syscall_getprocaddress(handle uintptr, name *uint8) (proc, err uintptr) {
	var errno uint32
	proc = c_getProcAddress(handle, name, &errno)
	return proc, uintptr(errno)
}
