package meta

// GlobalSummary is a whole-program metadata view over multiple PackageMetas,
// in one unified symbol/name space.
//
// Merge strategy:
//   - Symbols are interned into a global Symbol space; each package's local
//     symbols are mapped via locToGlb. Edges, TypeChildren, MethodSlots and
//     InterfaceMethods are NOT rewritten at merge time — they are translated
//     lazily on query. Only the strings are interned up front.
//   - Duplicate symbols (e.g. linkonce type descriptors emitted by several
//     packages) are assigned one owner by fact strength. Function-demand,
//     MethodInfo, InterfaceInfo and TypeChildren facts outrank ordinary edges,
//     so descriptor references do not hide the package that carries semantic
//     method/interface facts.
type GlobalSummary struct {
	pkgs []*PackageMeta

	// symbol space
	symIntern  map[string]Symbol
	symStrings []string   // Symbol → text
	locToGlb   [][]Symbol // [pkgIdx][localSym] → global Symbol
	owner      []symLoc   // global Symbol → owning (pkg, local); pkg<0 if none
	ownerRank  []uint8

	// method-name space (distinct from symbols)
	nameIntern  map[string]Name
	nameStrings []string // Name → text

	// per-type flags set at merge time (no translation, just CSR range checks)
	isInterface []bool // global Symbol → true if has iface methods

	// lazily translated, cached on first access
	methodInfo    map[Symbol][]GMethodSlot
	interfaceInfo map[Symbol][]GMethodSig

	interfaces []Symbol
}

// Symbol is a whole-program symbol ID in GlobalSummary's unified namespace.
type Symbol uint32

// Name is a whole-program method-name ID, in a namespace distinct from Symbol.
type Name uint32

// GMethodSlot is a method slot in the global namespace.
type GMethodSlot struct {
	Name  Name
	MType Symbol
	IFn   Symbol
	TFn   Symbol
}

// GMethodSig is an interface method signature in the global namespace.
type GMethodSig struct {
	Name  Name
	MType Symbol
}

// IfaceMethodDemand is a reachable interface method call: a demand that some
// type's method matching Sig on interface Target be kept.
type IfaceMethodDemand struct {
	Target Symbol
	Sig    GMethodSig
}

// symLoc identifies a (package, local symbol) pair. pkg < 0 means "no owner".
type symLoc struct {
	pkg   int32
	local LocalSymbol
}

// NewGlobalSummary merges package-local metadata into a whole-program view.
//
// Phase 1 interns all symbol names and builds the locToGlb mapping, owner
// indices, and type-kind flags. No per-symbol data is translated — only
// string interning and CSR range checks happen here.
//
// MethodSlots / InterfaceMethods / reflect are translated lazily on first
// access and cached. This avoids translating thousands of unused slots in
// the common case where DCE only reaches a fraction of all types.
func NewGlobalSummary(pkgs []*PackageMeta) (*GlobalSummary, error) {
	g := &GlobalSummary{
		pkgs:          pkgs,
		symIntern:     make(map[string]Symbol),
		nameIntern:    make(map[string]Name),
		locToGlb:      make([][]Symbol, len(pkgs)),
		methodInfo:    make(map[Symbol][]GMethodSlot),
		interfaceInfo: make(map[Symbol][]GMethodSig),
	}

	// Phase 1: intern symbols, build locToGlb and owner, mark type kinds.
	// Touches no edges, translates no slot/sig data.
	for pi, pm := range pkgs {
		if pm == nil {
			continue
		}
		n := pm.nsyms
		tab := make([]Symbol, n)
		for li := LocalSymbol(0); li < LocalSymbol(n); li++ {
			gs := g.internSymbol(pm.symbolName(li))
			tab[li] = gs
			rank := ownerRank(pm, li)
			if rank > g.ownerRank[gs] {
				g.owner[gs] = symLoc{pkg: int32(pi), local: li}
				g.ownerRank[gs] = rank
			}

			// mark type kinds (no translation, just CSR range checks)
			if pm.nifaceMethod(li) > 0 && !g.isInterface[gs] {
				g.isInterface[gs] = true
				g.interfaces = append(g.interfaces, gs)
			}
		}
		g.locToGlb[pi] = tab
	}
	return g, nil
}

// ownerRank reports whether li carries facts in pm and how authoritative those
// facts are for lazy global queries. Higher rank wins; equal rank keeps the
// first package, preserving deterministic first-wins behavior for equivalent
// duplicate ordinary/linkonce facts.
func ownerRank(pm *PackageMeta, li LocalSymbol) uint8 {
	switch {
	case pm.hasFuncDemand(li):
		return 5
	case pm.nmethodSlot(li) > 0:
		return 4
	case pm.nifaceMethod(li) > 0:
		return 3
	case pm.ntypeChild(li) > 0:
		return 2
	case pm.hasOrdinaryEdges(li):
		return 1
	default:
		return 0
	}
}

func (g *GlobalSummary) internSymbol(s string) Symbol {
	if id, ok := g.symIntern[s]; ok {
		return id
	}
	id := Symbol(len(g.symStrings))
	g.symIntern[s] = id
	g.symStrings = append(g.symStrings, s)
	g.owner = append(g.owner, symLoc{pkg: -1})
	g.ownerRank = append(g.ownerRank, 0)
	g.isInterface = append(g.isInterface, false)
	return id
}

func (g *GlobalSummary) internName(s string) Name {
	if id, ok := g.nameIntern[s]; ok {
		return id
	}
	id := Name(len(g.nameStrings))
	g.nameIntern[s] = id
	g.nameStrings = append(g.nameStrings, s)
	return id
}

// ownerData returns the owning package and locToGlb table for sym.
func (g *GlobalSummary) ownerData(sym Symbol) (*PackageMeta, []Symbol, LocalSymbol) {
	if int(sym) >= len(g.owner) {
		return nil, nil, 0
	}
	loc := g.owner[sym]
	if loc.pkg < 0 {
		return nil, nil, 0
	}
	return g.pkgs[loc.pkg], g.locToGlb[loc.pkg], loc.local
}

func (g *GlobalSummary) translateSlots(tab []Symbol, pm *PackageMeta, li LocalSymbol) []GMethodSlot {
	local := pm.methodSlots(li)
	out := make([]GMethodSlot, len(local))
	for i, s := range local {
		out[i] = GMethodSlot{
			Name:  g.internName(pm.nameString(s.Name)),
			MType: tab[s.MType],
			IFn:   tab[s.IFn],
			TFn:   tab[s.TFn],
		}
	}
	return out
}

func (g *GlobalSummary) translateSigs(tab []Symbol, pm *PackageMeta, li LocalSymbol) []GMethodSig {
	local := pm.ifaceMethods(li)
	out := make([]GMethodSig, len(local))
	for i, s := range local {
		out[i] = GMethodSig{
			Name:  g.internName(pm.nameString(s.Name)),
			MType: tab[s.MType],
		}
	}
	return out
}

// ── symbol / name identity ────────────────────────────────────────────────────

// LookupSymbol returns the global Symbol for a module-level symbol name.
func (g *GlobalSummary) LookupSymbol(name string) (Symbol, bool) {
	id, ok := g.symIntern[name]
	return id, ok
}

// SymbolName returns the text of a global Symbol.
func (g *GlobalSummary) SymbolName(sym Symbol) string {
	if int(sym) < len(g.symStrings) {
		return g.symStrings[sym]
	}
	return ""
}

// Name returns the text of a global Name.
func (g *GlobalSummary) Name(n Name) string {
	if int(n) < len(g.nameStrings) {
		return g.nameStrings[n]
	}
	return ""
}

// ── enumeration ───────────────────────────────────────────────────────────────

// Interfaces returns all interface type symbols.
func (g *GlobalSummary) Interfaces() []Symbol { return g.interfaces }

// ── lazy per-type queries ─────────────────────────────────────────────────────

// MethodSlots returns the ABI method slots for concrete type typ.
// Translated lazily on first access, cached thereafter.
func (g *GlobalSummary) MethodSlots(typ Symbol) []GMethodSlot {
	if slots, ok := g.methodInfo[typ]; ok {
		return slots
	}
	pm, tab, li := g.ownerData(typ)
	if pm == nil {
		return nil
	}
	slots := g.translateSlots(tab, pm, li)
	g.methodInfo[typ] = slots
	return slots
}

// InterfaceMethods returns the method set for interface iface.
// Translated lazily on first access, cached thereafter.
func (g *GlobalSummary) InterfaceMethods(iface Symbol) []GMethodSig {
	if sigs, ok := g.interfaceInfo[iface]; ok {
		return sigs
	}
	pm, tab, li := g.ownerData(iface)
	if pm == nil {
		return nil
	}
	sigs := g.translateSigs(tab, pm, li)
	g.interfaceInfo[iface] = sigs
	return sigs
}

// HasReflectMethod reports whether sym triggers conservative reflection handling.
func (g *GlobalSummary) HasReflectMethod(sym Symbol) bool {
	_, _, demands := g.ownerDemands(sym)
	for _, d := range demands {
		if d.Kind == DemandReflectMethod {
			return true
		}
	}
	return false
}

// ── lazy edge queries ─────────────────────────────────────────────────────────

// ownerOrdinary returns the owning package's locToGlb table and raw local
// ordinary edges for sym, or nil if sym has no owner.
func (g *GlobalSummary) ownerOrdinary(sym Symbol) ([]Symbol, []LocalSymbol) {
	pm, tab, li := g.ownerData(sym)
	if pm == nil {
		return nil, nil
	}
	return tab, pm.ordinaryEdges(li)
}

// ownerDemands returns the owning package, its locToGlb table, and raw local
// FuncDemand records for sym, or nil if sym has no owner.
func (g *GlobalSummary) ownerDemands(sym Symbol) (*PackageMeta, []Symbol, []FuncDemand) {
	pm, tab, li := g.ownerData(sym)
	if pm == nil {
		return nil, nil, nil
	}
	return pm, tab, pm.funcDemands(li)
}

// OrdinaryEdges returns plain reachability targets from sym (global Symbols).
func (g *GlobalSummary) OrdinaryEdges(sym Symbol) []Symbol {
	tab, edges := g.ownerOrdinary(sym)
	var out []Symbol
	for _, dst := range edges {
		out = append(out, tab[dst])
	}
	return out
}

// UseIface returns concrete types converted to interfaces by sym.
func (g *GlobalSummary) UseIface(sym Symbol) []Symbol {
	_, tab, demands := g.ownerDemands(sym)
	var out []Symbol
	for _, d := range demands {
		if d.Kind == DemandUseIface {
			out = append(out, tab[d.Target])
		}
	}
	return out
}

// UseIfaceMethod returns interface method demands emitted by sym.
func (g *GlobalSummary) UseIfaceMethod(sym Symbol) []IfaceMethodDemand {
	_, tab, demands := g.ownerDemands(sym)
	var out []IfaceMethodDemand
	for _, d := range demands {
		if d.Kind != DemandIfaceMethod {
			continue
		}
		iface := tab[d.Target]
		sigs := g.InterfaceMethods(iface)
		if int(d.Extra) < len(sigs) {
			out = append(out, IfaceMethodDemand{Target: iface, Sig: sigs[d.Extra]})
		}
	}
	return out
}

// UseNamedMethod returns constant MethodByName names referenced by sym.
func (g *GlobalSummary) UseNamedMethod(sym Symbol) []Name {
	pm, _, demands := g.ownerDemands(sym)
	if pm == nil {
		return nil
	}
	var out []Name
	for _, d := range demands {
		if d.Kind == DemandNamedMethod {
			name := pm.nameString(NameRef{Off: d.Target, Len: d.Extra})
			out = append(out, g.internName(name))
		}
	}
	return out
}

// TypeChildren returns child type symbols for typ (global Symbols).
func (g *GlobalSummary) TypeChildren(typ Symbol) []Symbol {
	pm, tab, li := g.ownerData(typ)
	if pm == nil {
		return nil
	}
	local := pm.typeChildren(li)
	if len(local) == 0 {
		return nil
	}
	out := make([]Symbol, len(local))
	for i, c := range local {
		out[i] = tab[c]
	}
	return out
}
