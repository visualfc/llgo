//go:build !windows || (!amd64 && !arm64)

package runtime

// LLVM entry-site anchors can land after the backend-generated function
// prologue. Targets without a link-phase rewrite retain a small look-behind
// window so function-value PCs still resolve to their metadata record.
const runtimeFuncPCEntrySlack = 64
