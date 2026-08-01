package runtime

import "unsafe"

func sysctlbynameInt32(name []byte) (int32, int32) {
	out := int32(0)
	nout := unsafe.Sizeof(out)
	ret := sysctlbyname(&name[0], (*byte)(unsafe.Pointer(&out)), &nout, nil, 0)
	return ret, out
}

func sysctlbynameBytes(name, out []byte) int32 {
	nout := uintptr(len(out))
	return sysctlbyname(&name[0], &out[0], &nout, nil, 0)
}

//go:linkname internal_cpu_getsysctlbyname internal/cpu.getsysctlbyname
func internal_cpu_getsysctlbyname(name []byte) (int32, int32) {
	return sysctlbynameInt32(name)
}

//go:linkname internal_cpu_sysctlbynameInt32 internal/cpu.sysctlbynameInt32
func internal_cpu_sysctlbynameInt32(name []byte) (int32, int32) {
	return sysctlbynameInt32(name)
}

//go:linkname internal_cpu_sysctlbynameBytes internal/cpu.sysctlbynameBytes
func internal_cpu_sysctlbynameBytes(name, out []byte) int32 {
	return sysctlbynameBytes(name, out)
}

//go:cgo_import_dynamic libc_sysctlbyname sysctlbyname "/usr/lib/libSystem.B.dylib"
var libc_sysctlbyname_trampoline_addr uintptr

// adapted from runtime/sys_darwin.go in the pattern of sysctl() above, as defined in x/sys/unix
func sysctlbyname(name *byte, old *byte, oldlen *uintptr, new *byte, newlen uintptr) int32 {
	r, _, _ := llgo_rawSyscall6(
		libc_sysctlbyname_trampoline_addr,
		uintptr(unsafe.Pointer(name)),
		uintptr(unsafe.Pointer(old)),
		uintptr(unsafe.Pointer(oldlen)),
		uintptr(unsafe.Pointer(new)),
		uintptr(newlen),
		0,
	)
	return int32(r)
}
