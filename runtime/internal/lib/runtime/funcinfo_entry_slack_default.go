//go:build !windows || (!amd64 && !arm64)

package runtime

// LLVM entry-site anchors can land after the backend-generated function
// prologue. Targets without Win64 unwind metadata retain a small look-behind
// window so function-value PCs still resolve to their metadata record.
const runtimeFuncPCEntrySlack = 64

func runtimeFuncPCMayUseEntrySlack(uintptr) bool {
	return true
}
