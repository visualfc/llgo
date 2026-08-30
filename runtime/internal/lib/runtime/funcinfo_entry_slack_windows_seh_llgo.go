//go:build windows && (amd64 || arm64)

package runtime

// LLVM entry-site anchors land after the backend-generated prologue, so exact
// function-value PCs still need the same small look-ahead window as other
// targets. Before using it, consult PE unwind metadata: a PC known to be inside
// a non-leaf function is an ordinary instruction address, not a function entry,
// and must not resolve to the next densely packed function's anchor.
const runtimeFuncPCEntrySlack = 64
