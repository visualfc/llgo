//go:build baremetal || wasm || (!darwin && !linux && !windows) || ((darwin || linux) && !amd64 && !arm64) || (windows && !386 && !amd64 && !arm64)

package runtime

func SetCPUProfileRate(hz int) {}

func cpuProfileSignalLock(sig uint32) bool { return false }

func cpuProfileSignalUnlock(locked bool) {}
