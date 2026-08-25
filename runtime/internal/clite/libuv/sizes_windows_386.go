//go:build windows && 386

package libuv

// libuv 1.x public handle sizes on 32-bit Windows. The structures are opaque;
// reserve their complete storage so libuv never writes past the Go wrapper.
const (
	uvHandleSize = 48
	uvAsyncSize  = 116
	uvTimerSize  = 96
	uvSignalSize = 136
)
