//go:build !baremetal && !wasm && (darwin || linux) && (amd64 || arm64)

package runtime

import _ "unsafe"

//go:linkname c_cpuProfileUpdateSignal C.llgo_cpu_profile_update_signal
func c_cpuProfileUpdateSignal(mode int32) int32

func cpuProfileSignalUpdate(sig uint32, mode int32) (handled bool, code int) {
	if sig != cpuProfileSignal {
		return false, 0
	}
	result := c_cpuProfileUpdateSignal(mode)
	if result == 0 {
		return false, 0
	}
	if result < 0 {
		return true, int(-result)
	}
	return true, 0
}
