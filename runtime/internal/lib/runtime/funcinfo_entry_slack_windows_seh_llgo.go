//go:build windows && (amd64 || arm64)

package runtime

// LLVM entry-site anchors land after the backend-generated prologue, so exact
// function-value PCs still need the same small look-ahead window as other
// targets. Before using it, consult PE unwind metadata: a PC known to be inside
// a non-leaf function is an ordinary instruction address, not a function entry,
// and must not resolve to the next densely packed function's anchor.
const runtimeFuncPCEntrySlack = 64

func runtimeFuncPCMayUseEntrySlack(pc uintptr) bool {
	var imageBase uintptr
	entry := c_windowsLookupFunctionEntry(pc, &imageBase)
	if entry == nil {
		// Leaf functions have no RUNTIME_FUNCTION record. Preserve the anchor
		// fallback so their function values remain symbolizable.
		return true
	}
	begin := imageBase + uintptr(*(*uint32)(entry))
	return begin == pc
}
