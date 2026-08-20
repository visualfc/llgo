//go:build windows

package runtime

// The fault handler belongs to the core runtime: nil deferred calls and other
// compiler-generated panics must work even when package runtime is not linked.
const platformFaultLLGoFiles = "; _wrap/fault_windows.c"
