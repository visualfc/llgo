//go:build baremetal || wasm || (!darwin && !linux) || (!amd64 && !arm64)

package runtime

func SetCPUProfileRate(hz int) {}

func cpuProfileSignalUpdateBegin(sig uint32) bool { return false }

func cpuProfileSignalUpdateEnd(locked bool) {}
