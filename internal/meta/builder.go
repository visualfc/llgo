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
	symNames []symEntry        // indexed by Symbol
	symMap   map[string]Symbol // name → Symbol

	// per-symbol ordinary edge lists (source Symbol → target package-local Symbols)
	ordinaryEdges [][]Symbol

	// per-symbol function demand lists (source Symbol → demand facts)
	funcDemands [][]bFuncDemand

	// per-symbol TypeChildren lists
	typeChildren    [][]Symbol
	typeChildrenSet map[[2]Symbol]struct{} // dedup (parent, child) pairs

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
	kind   DemandKind
	target uint32 // Symbol or stringTable offset (DemandNamedMethod)
	extra  uint32
}

type bMethodSlot struct {
	name  nameRef // method short name
	mtype uint32  // Symbol
	ifn   uint32  // Symbol
	tfn   uint32  // Symbol
}

type bMethodSig struct {
	name  nameRef // method short name
	mtype uint32  // Symbol
}

// NewBuilder creates an empty Builder.
func NewBuilder() *Builder {
	return &Builder{
		strMap:          make(map[string]uint32),
		symMap:          make(map[string]Symbol),
		typeChildrenSet: make(map[[2]Symbol]struct{}),
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

// internName registers a name string and returns a nameRef.
func (b *Builder) internName(s string) nameRef {
	return nameRef{Off: b.internStr(s), Len: uint32(len(s))}
}

// Sym registers a symbol by name and returns its Symbol.
// Calling Sym with the same name twice returns the same Symbol.
// Whether the symbol is defined in this package or referenced from another
// makes no difference to the metadata format.
func (b *Builder) Sym(name string) Symbol {
	return b.sym(name)
}

func (b *Builder) sym(name string) Symbol {
	if id, ok := b.symMap[name]; ok {
		return id
	}
	id := Symbol(len(b.symNames))
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

// AddOrdinaryEdge records a plain symbol-to-symbol reference from src to dst
// (call, type use, global var reference, etc.).
func (b *Builder) AddOrdinaryEdge(src, dst Symbol) {
	b.ordinaryEdges[src] = append(b.ordinaryEdges[src], dst)
}

// AddIfaceUse records that src converts a value of type typ to an interface.
func (b *Builder) AddIfaceUse(src, typ Symbol) {
	b.funcDemands[src] = append(b.funcDemands[src], bFuncDemand{
		kind:   DemandUseIface,
		target: uint32(typ),
	})
}

// AddIfaceMethodUse records that src calls the methodIndex-th method (in
// declaration order) of interface iface.
func (b *Builder) AddIfaceMethodUse(src, iface Symbol, methodIndex uint32) {
	b.funcDemands[src] = append(b.funcDemands[src], bFuncDemand{
		kind:   DemandIfaceMethod,
		target: uint32(iface),
		extra:  methodIndex,
	})
}

// AddNamedMethodUse records that src does a constant MethodByName(methodName)
// call. The method name is stored as a string-table reference.
func (b *Builder) AddNamedMethodUse(src Symbol, methodName string) {
	ref := b.internName(methodName)
	b.funcDemands[src] = append(b.funcDemands[src], bFuncDemand{
		kind:   DemandNamedMethod,
		target: ref.Off,
		extra:  ref.Len,
	})
}

// AddTypeChild records that parent type structurally contains child type.
// Idempotent: duplicate (parent, child) pairs are silently ignored.
func (b *Builder) AddTypeChild(parent, child Symbol) {
	key := [2]Symbol{parent, child}
	if _, ok := b.typeChildrenSet[key]; ok {
		return
	}
	b.typeChildrenSet[key] = struct{}{}
	b.typeChildren[parent] = append(b.typeChildren[parent], child)
}

// AddMethodSlot records one ABI method slot for a concrete type.
// Slots must be appended in abi.Method table order.
func (b *Builder) AddMethodSlot(typ Symbol, methodName string, mtype, ifn, tfn Symbol) {
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
func (b *Builder) AddIfaceMethod(iface Symbol, methodName string, mtype Symbol) {
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
func (b *Builder) MarkReflect(sym Symbol) {
	b.funcDemands[sym] = append(b.funcDemands[sym], bFuncDemand{
		kind: DemandReflectMethod,
	})
}
