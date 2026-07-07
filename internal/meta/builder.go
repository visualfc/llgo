package meta

// Builder accumulates per-package metadata facts and serializes them into
// the binary wire format understood by PackageMeta.
//
// Typical usage:
//
//	b := NewBuilder()
//	fn := b.Sym("main.main")
//	callee := b.Sym("fmt.Println")
//	b.AddOrdinaryEdge(fn, callee)
//	pm, err := b.Build()
type Builder struct {
	// string interning
	strData []byte            // raw byte stream, all strings concatenated
	strMap  map[string]uint32 // string → offset in strData

	// symbol table
	symNames []symEntry             // indexed by LocalSymbol
	symMap   map[string]LocalSymbol // name → LocalSymbol

	// per-symbol ordinary edge lists (source LocalSymbol → target LocalSymbols)
	ordinaryEdges [][]LocalSymbol

	// per-symbol function demand lists (source LocalSymbol → demand facts)
	funcDemands [][]bFuncDemand

	// per-symbol TypeChildren lists
	typeChildren    [][]LocalSymbol
	typeChildrenSet map[[2]LocalSymbol]struct{} // dedup (parent, child) pairs

	// per-symbol MethodInfo (only concrete types)
	methodInfo [][]bMethodSlot

	// per-symbol InterfaceInfo (only interface types)
	ifaceInfo [][]bMethodSig
}

type symEntry struct {
	nameOff uint32
	nameLen uint32
}

type bFuncDemand struct {
	kind   uint32
	target uint32 // LocalSymbol or stringTable offset (DemandNamedMethod)
	extra  uint32
}

type bMethodSlot struct {
	name  NameRef // method short name
	mtype uint32  // LocalSymbol
	ifn   uint32  // LocalSymbol
	tfn   uint32  // LocalSymbol
}

type bMethodSig struct {
	name  NameRef // method short name
	mtype uint32  // LocalSymbol
}

// NewBuilder creates an empty Builder.
func NewBuilder() *Builder {
	return &Builder{
		strMap:          make(map[string]uint32),
		symMap:          make(map[string]LocalSymbol),
		typeChildrenSet: make(map[[2]LocalSymbol]struct{}),
	}
}

// internStr adds s to the string byte stream (idempotent) and returns its offset.
func (b *Builder) internStr(s string) uint32 {
	if off, ok := b.strMap[s]; ok {
		return off
	}
	off := uint32(len(b.strData))
	b.strData = append(b.strData, s...)
	b.strMap[s] = off
	return off
}

// internName registers a name string and returns a NameRef.
func (b *Builder) internName(s string) NameRef {
	return NameRef{Off: b.internStr(s), Len: uint32(len(s))}
}

// Sym registers a symbol by name and returns its LocalSymbol.
// Calling Sym with the same name twice returns the same LocalSymbol.
// Whether the symbol is defined in this package or referenced from another
// makes no difference to the metadata format.
func (b *Builder) Sym(name string) LocalSymbol {
	return b.sym(name)
}

func (b *Builder) sym(name string) LocalSymbol {
	if id, ok := b.symMap[name]; ok {
		return id
	}
	id := LocalSymbol(len(b.symNames))
	off := b.internStr(name)
	b.symNames = append(b.symNames, symEntry{nameOff: off, nameLen: uint32(len(name))})
	b.symMap[name] = id
	// grow all per-symbol structures in sync with the symbol table
	b.ordinaryEdges = append(b.ordinaryEdges, nil)
	b.funcDemands = append(b.funcDemands, nil)
	b.typeChildren = append(b.typeChildren, nil)
	b.methodInfo = append(b.methodInfo, nil)
	b.ifaceInfo = append(b.ifaceInfo, nil)
	return id
}

// addEdge records a directed edge from src to dst with the given kind and
// extra. This is a private dispatch helper — external callers (and even most
// callers within this package) should use one of the typed methods below
// (AddOrdinaryEdge, AddIfaceUse, AddIfaceMethodUse, AddNamedMethodEdge)
// instead, so they never need to know about the wire-format kind tags.
//
//   - edgeOrdinary:       dst is a LocalSymbol; extra = 0
//   - edgeUseIface:       dst is a LocalSymbol (type); extra = 0
//   - edgeUseIfaceMethod: dst is a LocalSymbol (interface); extra = method index
//   - edgeUseNamedMethod: dst is a string-table offset; extra = string length
func (b *Builder) addEdge(src, dst LocalSymbol, kind uint8, extra uint32) {
	switch kind {
	case edgeOrdinary:
		b.ordinaryEdges[src] = append(b.ordinaryEdges[src], dst)
	case edgeUseIface:
		b.funcDemands[src] = append(b.funcDemands[src], bFuncDemand{
			kind:   DemandUseIface,
			target: uint32(dst),
			extra:  extra,
		})
	case edgeUseIfaceMethod:
		b.funcDemands[src] = append(b.funcDemands[src], bFuncDemand{
			kind:   DemandIfaceMethod,
			target: uint32(dst),
			extra:  extra,
		})
	case edgeUseNamedMethod:
		b.funcDemands[src] = append(b.funcDemands[src], bFuncDemand{
			kind:   DemandNamedMethod,
			target: uint32(dst),
			extra:  extra,
		})
	}
}

// AddOrdinaryEdge records a plain symbol-to-symbol reference from src to dst
// (call, type use, global var reference, etc.).
func (b *Builder) AddOrdinaryEdge(src, dst LocalSymbol) {
	b.addEdge(src, dst, edgeOrdinary, 0)
}

// AddIfaceUse records that src converts a value of type typ to an interface.
func (b *Builder) AddIfaceUse(src, typ LocalSymbol) {
	b.addEdge(src, typ, edgeUseIface, 0)
}

// AddIfaceMethodUse records that src calls the methodIndex-th method (in
// declaration order) of interface iface.
func (b *Builder) AddIfaceMethodUse(src, iface LocalSymbol, methodIndex uint32) {
	b.addEdge(src, iface, edgeUseIfaceMethod, methodIndex)
}

// AddNamedMethodEdge records that src does a constant MethodByName(methodName)
// call. Unlike the other typed Add*Use methods, the target here is a method
// name string rather than a LocalSymbol: its byte offset and length in the
// string table are stored together as a NameRef.
func (b *Builder) AddNamedMethodEdge(src LocalSymbol, methodName string) {
	ref := b.internName(methodName)
	b.funcDemands[src] = append(b.funcDemands[src], bFuncDemand{
		kind:   DemandNamedMethod,
		target: ref.Off,
		extra:  ref.Len,
	})
}

// AddTypeChild records that parent type structurally contains child type.
// Idempotent: duplicate (parent, child) pairs are silently ignored.
func (b *Builder) AddTypeChild(parent, child LocalSymbol) {
	key := [2]LocalSymbol{parent, child}
	if _, ok := b.typeChildrenSet[key]; ok {
		return
	}
	b.typeChildrenSet[key] = struct{}{}
	b.typeChildren[parent] = append(b.typeChildren[parent], child)
}

// AddMethodSlot records one ABI method slot for a concrete type.
// Slots must be appended in abi.Method table order.
func (b *Builder) AddMethodSlot(typ LocalSymbol, methodName string, mtype, ifn, tfn LocalSymbol) {
	b.methodInfo[typ] = append(b.methodInfo[typ], bMethodSlot{
		name:  b.internName(methodName),
		mtype: uint32(mtype),
		ifn:   uint32(ifn),
		tfn:   uint32(tfn),
	})
}

// AddIfaceMethod records one method in an interface's method set.
// Idempotent: if the same (name, mtype) pair is already registered for iface,
// this call is a no-op — the Builder deduplicates internally.
func (b *Builder) AddIfaceMethod(iface LocalSymbol, methodName string, mtype LocalSymbol) {
	ref := b.internName(methodName)
	mt := uint32(mtype)
	for _, s := range b.ifaceInfo[iface] {
		if s.name == ref && s.mtype == mt {
			return
		}
	}
	b.ifaceInfo[iface] = append(b.ifaceInfo[iface], bMethodSig{
		name:  ref,
		mtype: mt,
	})
}

// MarkReflect marks sym as triggering conservative reflection handling.
func (b *Builder) MarkReflect(sym LocalSymbol) {
	b.funcDemands[sym] = append(b.funcDemands[sym], bFuncDemand{
		kind: DemandReflectMethod,
	})
}
