//go:build windows && 386

package ffi

// WindowsStdcallABI is libffi's stdcall ABI on 32-bit Windows. Its numeric
// value is part of libffi's x86 ABI enumeration; _wrap/libffi.c verifies it
// against the installed headers.
const WindowsStdcallABI ABI = 2 // FFI_STDCALL
