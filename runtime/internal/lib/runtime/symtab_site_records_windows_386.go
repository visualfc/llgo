//go:build windows && 386

package runtime

// LLVM follows the Windows C ABI and aligns uint64 fields to 8 bytes on 386,
// while Go aligns them to 4. These explicit words make the Go declarations
// match the 16-byte COFF metadata records emitted by LLGo without changing
// the metadata or runtime layout on any other target.
type runtimeFuncInfoSymbolIndexRecord struct {
	symbolID  uint64
	funcIndex uint32
	_         uint32
}

type runtimeFuncInfoEntryRecord struct {
	pc uintptr
	_  uint32

	symbolID uint64
}

type runtimePCSiteRecord struct {
	pc uintptr
	_  uint32

	id uint64
}
