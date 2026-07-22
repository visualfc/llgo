package runtime

import "unsafe"

func sysctlbynameInt32(name []byte) (int32, int32) {
	out := int32(0)
	nout := unsafe.Sizeof(out)
	ret := sysctlbyname(&name[0], (*byte)(unsafe.Pointer(&out)), &nout, nil, 0)
	return ret, out
}

//go:linkname internal_cpu_getsysctlbyname internal/cpu.getsysctlbyname
func internal_cpu_getsysctlbyname(name []byte) (int32, int32) {
	return sysctlbynameInt32(name)
}

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
