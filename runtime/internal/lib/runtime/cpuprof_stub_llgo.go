//go:build baremetal || wasm || (!darwin && !linux && !windows) || (!amd64 && !arm64)

package runtime

func SetCPUProfileRate(hz int) {}

func cpuProfileSignalLock(sig uint32) bool { return false }

func cpuProfileSignalUnlock(locked bool) {}
