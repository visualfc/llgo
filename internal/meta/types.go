// Package meta defines the binary format and in-memory view for LLGo package
// summary cache files (.meta). The format is designed for zero-copy access via
// mmap: the file layout is the memory layout.
package meta

// LocalSymbol is a package-local symbol ID, equal to its index in the Symbols
// section. Valid within one PackageMeta only; use GlobalSummary for cross-package
// references.
type LocalSymbol uint32

// nameRef references a name string by its byte range in the string table.
// It is used for method short names, which are matched by value across packages
// — names are not module-level symbols and live in their own namespace.
type nameRef struct {
	Off uint32
	Len uint32
}

// Edge/demand kinds used internally by Builder.addEdge to pick which wire
// section a fact is written to. These are pure wire-format encoding details:
// callers outside this package never see them — they call the typed Builder
// methods (AddOrdinaryEdge, AddIfaceUse, AddIfaceMethodUse, AddNamedMethodEdge)
// instead.
const (
	// edgeOrdinary is a plain symbol reference (call, type use, global var, etc.).
	edgeOrdinary uint8 = 0
	// edgeUseIface marks that the source converts Target type to an interface.
	edgeUseIface uint8 = 1
	// edgeUseIfaceMethod marks a call to method Extra of interface Target.
	edgeUseIfaceMethod uint8 = 2
	// edgeUseNamedMethod marks a constant MethodByName call; Target is a
	// stringTable byte offset (not a LocalSymbol).
	edgeUseNamedMethod uint8 = 3
)

// DemandKind identifies a function-level method/interface/reflection demand.
type DemandKind uint32

// FuncDemand kinds used in the FuncDemand section and global analysis API.
const (
	DemandUseIface      DemandKind = 1
	DemandIfaceMethod   DemandKind = 2
	DemandNamedMethod   DemandKind = 3
	DemandReflectMethod DemandKind = 4
)

// magic is the 4-byte file signature.
const magic = "LLPM"

// version is the current binary format version.
const version = 2

// Section index constants for the wire-format header.
const (
	secStringTable   = 0
	secSymbols       = 1
	secOrdinaryEdges = 2
	secFuncDemand    = 3
	secTypeChildren  = 4
	secMethodInfo    = 5
	secIfaceInfo     = 6
	numSections      = 7
)

// headerSize = magic(4) + version(4) + sectionOffsets(numSections×4)
const headerSize = 4 + 4 + numSections*4
