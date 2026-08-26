//go:build windows && (amd64 || arm64)

package libuv

// libuv 1.x public handle sizes on 64-bit Windows. The structures are opaque;
// reserve their complete storage so libuv never writes past the Go wrapper.
const (
	uvHandleSize = 96
	uvAsyncSize  = 224
	uvTimerSize  = 160
	uvSignalSize = 264
)
