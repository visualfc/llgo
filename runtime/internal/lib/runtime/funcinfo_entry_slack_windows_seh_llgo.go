//go:build windows && (amd64 || arm64)

package runtime

// Win64 COFF entry records use the exact function symbol. A non-zero window
// would let an ordinary PC near the end of one function resolve as the entry
// of the next densely packed function.
const runtimeFuncPCEntrySlack = 0
