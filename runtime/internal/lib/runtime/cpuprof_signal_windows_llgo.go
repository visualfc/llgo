//go:build !baremetal && windows && (amd64 || arm64)

package runtime

// Windows CPU profiling uses a sampler thread instead of a process signal.
// The Windows os/signal backend therefore never calls these coordination
// helpers; keep the sentinel outside its supported console-signal range.
const cpuProfileSignal = ^uint32(0)
