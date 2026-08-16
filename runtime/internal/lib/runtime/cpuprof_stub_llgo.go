//go:build baremetal || wasm || (!darwin && !linux) || (!amd64 && !arm64)

package runtime

func SetCPUProfileRate(hz int) {}
