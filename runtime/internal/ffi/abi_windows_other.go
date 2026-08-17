//go:build windows && !386

package ffi

// Windows has a single native calling convention on amd64 and arm64.
const WindowsStdcallABI ABI = DefaultABI
