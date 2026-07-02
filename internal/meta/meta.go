package meta

import (
	"encoding/binary"
	"fmt"
	"os"
	"syscall"
	"unsafe"
)

// PackageMeta is a zero-copy view over a .meta file byte slice.
// The underlying bytes may come from an mmap'd file or from Builder.Build().
// All query methods read directly from the byte slice with no allocation.
type PackageMeta struct {
	raw  []byte
	mmap bool // true → must Munmap on Close

	nsyms uint32

	// cached section start offsets (parsed once from header)
	strOff      uint32
	symOff      uint32
	ordinaryOff uint32
	demandOff   uint32
	childOff    uint32
	methodOff   uint32
	ifaceOff    uint32
}

// FuncDemand is a decoded function-demand record. Its in-memory layout
// (Kind@0, Target@4, Extra@8, size 12) must match the on-disk wire layout exactly
// so funcDemands can reinterpret the mmap bytes as []FuncDemand with no copy.
type FuncDemand struct {
	Kind   uint32
	Target uint32 // LocalSymbol or stringTable offset (DemandNamedMethod)
	Extra  uint32
}

// Compile-time assertion: FuncDemand must be exactly 12 bytes. If either const
// goes negative the build fails, pinning the wire/struct layout match.
const (
	_ = uint(unsafe.Sizeof(FuncDemand{}) - 12)
	_ = uint(12 - unsafe.Sizeof(FuncDemand{}))
)

// MethodSlot is a decoded method slot record. Its layout (NameRef@0..8,
// MType@8, IFn@12, TFn@16, size 20) must match the on-disk wire layout for
// zero-copy reads.
type MethodSlot struct {
	Name  NameRef // method short name
	MType LocalSymbol
	IFn   LocalSymbol
	TFn   LocalSymbol
}

// MethodSig is a decoded interface method signature. Layout: NameRef@0..8,
// MType@8, size 12 — must match the on-disk wire layout for zero-copy reads.
type MethodSig struct {
	Name  NameRef // method short name
	MType LocalSymbol
}

// Compile-time assertions pinning the wire/struct layout for zero-copy reads.
// If a struct's size drifts, one of these uint consts goes negative and the
// build fails.
const (
	_ = uint(unsafe.Sizeof(MethodSlot{}) - 20)
	_ = uint(20 - unsafe.Sizeof(MethodSlot{}))
	_ = uint(unsafe.Sizeof(MethodSig{}) - 12)
	_ = uint(12 - unsafe.Sizeof(MethodSig{}))
)

// ReadMeta opens path, mmaps it, and returns a PackageMeta view.
// Call Close when done to release the mapping.
func ReadMeta(path string) (*PackageMeta, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	fi, err := f.Stat()
	if err != nil {
		return nil, err
	}
	size := int(fi.Size())
	if size < headerSize {
		return nil, fmt.Errorf("meta: file too small: %s", path)
	}

	raw, err := syscall.Mmap(int(f.Fd()), 0, size, syscall.PROT_READ, syscall.MAP_SHARED)
	if err != nil {
		return nil, fmt.Errorf("meta: mmap %s: %w", path, err)
	}

	pm, err := newPackageMeta(raw)
	if err != nil {
		_ = syscall.Munmap(raw)
		return nil, err
	}
	pm.mmap = true
	return pm, nil
}

// Bytes returns the underlying raw byte slice (for writing to disk).
func (pm *PackageMeta) Bytes() []byte { return pm.raw }

// Close releases the mmap mapping if one was used.
func (pm *PackageMeta) Close() error {
	if pm.mmap && pm.raw != nil {
		err := syscall.Munmap(pm.raw)
		pm.raw = nil
		return err
	}
	return nil
}

// symbolName returns the name of sym as a zero-copy view into the string table.
// The returned string points directly into the mmap region and is only valid for
// the lifetime of pm — do not retain it after Close.
func (pm *PackageMeta) symbolName(sym LocalSymbol) string {
	if uint32(sym) >= pm.nsyms {
		return ""
	}
	const recSize = 12
	base := pm.symOff + 4 + uint32(sym)*recSize
	nameOff := binary.LittleEndian.Uint32(pm.raw[base+0:])
	nameLen := binary.LittleEndian.Uint32(pm.raw[base+4:])
	return unsafe.String(&pm.raw[pm.strOff+nameOff], int(nameLen))
}

// NameString returns the string referenced by a NameRef as a zero-copy view
// into the string table. The returned string points directly into the mmap
// region and is only valid for the lifetime of pm — do not retain it after Close.
func (pm *PackageMeta) nameString(ref NameRef) string {
	return unsafe.String(&pm.raw[pm.strOff+ref.Off], int(ref.Len))
}

// NOrdinaryEdge returns the number of plain reachability edges from sym.
func (pm *PackageMeta) nordinaryEdge(sym LocalSymbol) uint32 {
	s, e := pm.csrRange(pm.ordinaryOff, sym)
	return e - s
}

// ordinaryEdges returns all plain reachability targets from sym as a zero-copy
// view into the mmap region.
func (pm *PackageMeta) ordinaryEdges(sym LocalSymbol) []LocalSymbol {
	return csrSlice[LocalSymbol](pm, pm.ordinaryOff, sym, 4)
}

// NFuncDemand returns the number of method/interface/reflection demands from sym.
func (pm *PackageMeta) nfuncDemand(sym LocalSymbol) uint32 {
	s, e := pm.csrRange(pm.demandOff, sym)
	return e - s
}

// funcDemands returns all method/interface/reflection demands from sym as a
// zero-copy view into the mmap region.
func (pm *PackageMeta) funcDemands(sym LocalSymbol) []FuncDemand {
	return csrSlice[FuncDemand](pm, pm.demandOff, sym, 12)
}

// NTypeChild returns the number of type children for sym, or 0 if none.
func (pm *PackageMeta) ntypeChild(sym LocalSymbol) uint32 {
	s, e := pm.csrRange(pm.childOff, sym)
	return e - s
}

// TypeChildren returns the child type LocalSymbols for sym as a zero-copy view
// into the mmap region.
func (pm *PackageMeta) typeChildren(sym LocalSymbol) []LocalSymbol {
	return csrSlice[LocalSymbol](pm, pm.childOff, sym, 4)
}

// NMethodSlot returns the number of ABI method slots for sym, or 0 if none.
func (pm *PackageMeta) nmethodSlot(sym LocalSymbol) uint32 {
	s, e := pm.csrRange(pm.methodOff, sym)
	return e - s
}

// MethodSlots returns the ABI method slots for concrete type sym as a zero-copy
// view into the mmap region.
func (pm *PackageMeta) methodSlots(sym LocalSymbol) []MethodSlot {
	return csrSlice[MethodSlot](pm, pm.methodOff, sym, 20)
}

// NIfaceMethod returns the number of methods in an interface, or 0 if sym is
// not an interface.
func (pm *PackageMeta) nifaceMethod(sym LocalSymbol) uint32 {
	s, e := pm.csrRange(pm.ifaceOff, sym)
	return e - s
}

// IfaceMethods returns the method signatures for interface sym as a zero-copy
// view into the mmap region.
func (pm *PackageMeta) ifaceMethods(sym LocalSymbol) []MethodSig {
	return csrSlice[MethodSig](pm, pm.ifaceOff, sym, 12)
}

// HasReflect reports whether sym triggers conservative reflection handling.
func (pm *PackageMeta) hasReflect(sym LocalSymbol) bool {
	for _, d := range pm.funcDemands(sym) {
		if d.Kind == DemandReflectMethod {
			return true
		}
	}
	return false
}

// HasOrdinaryEdges reports whether sym has any plain reachability edges.
func (pm *PackageMeta) hasOrdinaryEdges(sym LocalSymbol) bool {
	return pm.nordinaryEdge(sym) > 0
}

// HasFuncDemand reports whether sym has any method/interface/reflection demand.
func (pm *PackageMeta) hasFuncDemand(sym LocalSymbol) bool {
	return pm.nfuncDemand(sym) > 0
}

// ── internal helpers ──────────────────────────────────────────────────────────

// newPackageMeta parses the header of raw and returns a PackageMeta.
func newPackageMeta(raw []byte) (*PackageMeta, error) {
	if len(raw) < headerSize {
		return nil, fmt.Errorf("meta: raw too small (%d bytes)", len(raw))
	}
	if string(raw[0:4]) != Magic {
		return nil, fmt.Errorf("meta: bad magic %q", raw[0:4])
	}
	ver := binary.LittleEndian.Uint32(raw[4:8])
	if ver != Version {
		return nil, fmt.Errorf("meta: unsupported version %d", ver)
	}

	pm := &PackageMeta{raw: raw}
	pm.strOff = binary.LittleEndian.Uint32(raw[8+SecStringTable*4:])
	pm.symOff = binary.LittleEndian.Uint32(raw[8+SecSymbols*4:])
	pm.ordinaryOff = binary.LittleEndian.Uint32(raw[8+SecOrdinaryEdges*4:])
	pm.demandOff = binary.LittleEndian.Uint32(raw[8+SecFuncDemand*4:])
	pm.childOff = binary.LittleEndian.Uint32(raw[8+SecTypeChildren*4:])
	pm.methodOff = binary.LittleEndian.Uint32(raw[8+SecMethodInfo*4:])
	pm.ifaceOff = binary.LittleEndian.Uint32(raw[8+SecIfaceInfo*4:])

	// read nsyms from Symbols section header
	pm.nsyms = binary.LittleEndian.Uint32(raw[pm.symOff:])
	return pm, nil
}

// csrSlice returns a zero-copy []T view into a CSR section. recSize is the
// on-disk size of one record (must match unsafe.Sizeof(T)).
func csrSlice[T any](pm *PackageMeta, sectionOff uint32, sym LocalSymbol, recSize uintptr) []T {
	if uint32(sym) >= pm.nsyms {
		return nil
	}
	start, end := pm.csrRange(sectionOff, sym)
	if start == end {
		return nil
	}
	dataBase := sectionOff + 4 + (pm.nsyms+1)*4
	p := (*T)(unsafe.Pointer(&pm.raw[dataBase+uint32(uintptr(start)*recSize)]))
	return unsafe.Slice(p, end-start)
}

func (pm *PackageMeta) csrRange(sectionOff uint32, sym LocalSymbol) (start, end uint32) {
	offsetsBase := sectionOff + 4 // skip nsyms u32
	start = binary.LittleEndian.Uint32(pm.raw[offsetsBase+uint32(sym)*4:])
	end = binary.LittleEndian.Uint32(pm.raw[offsetsBase+(uint32(sym)+1)*4:])
	return
}
