//go:build !windows || !386

package runtime

type runtimeFuncInfoSymbolIndexRecord struct {
	symbolID  uint64
	funcIndex uint32
}

type runtimeFuncInfoEntryRecord struct {
	pc       uintptr
	symbolID uint64
}

type runtimePCSiteRecord struct {
	pc uintptr
	id uint64
}
