package ssa

import (
	"go/types"

	"github.com/xgo-dev/llvm"
)

const (
	vcallVisibilityLinkageUnit = 1
	moduleFlagBehaviorMin      = 8
)

func methodCapabilitySig(sig *types.Signature) string {
	canon := types.NewSignatureType(nil, nil, nil, methodCapabilityTuple(sig.Params()), methodCapabilityTuple(sig.Results()), sig.Variadic())
	return types.TypeString(canon, func(pkg *types.Package) string {
		if pkg == nil {
			return ""
		}
		return PathOf(pkg)
	})
}

func methodCapabilityTuple(tuple *types.Tuple) *types.Tuple {
	if tuple == nil || tuple.Len() == 0 {
		return nil
	}
	vars := make([]*types.Var, tuple.Len())
	for i := range vars {
		v := tuple.At(i)
		vars[i] = types.NewVar(v.Pos(), v.Pkg(), "", methodCapabilityType(v.Type()))
	}
	return types.NewTuple(vars...)
}

func methodCapabilityType(t types.Type) types.Type {
	t = types.Unalias(t)
	switch t := t.(type) {
	case *types.Array:
		return types.NewArray(methodCapabilityType(t.Elem()), t.Len())
	case *types.Slice:
		return types.NewSlice(methodCapabilityType(t.Elem()))
	case *types.Struct:
		fields := make([]*types.Var, t.NumFields())
		tags := make([]string, t.NumFields())
		for i := range fields {
			f := t.Field(i)
			fields[i] = types.NewField(f.Pos(), f.Pkg(), f.Name(), methodCapabilityType(f.Type()), f.Embedded())
			tags[i] = t.Tag(i)
		}
		return types.NewStruct(fields, tags)
	case *types.Pointer:
		return types.NewPointer(methodCapabilityType(t.Elem()))
	case *types.Signature:
		return types.NewSignatureType(nil, nil, nil, methodCapabilityTuple(t.Params()), methodCapabilityTuple(t.Results()), t.Variadic())
	case *types.Interface:
		methods := make([]*types.Func, t.NumExplicitMethods())
		for i := range methods {
			m := t.ExplicitMethod(i)
			methods[i] = types.NewFunc(m.Pos(), m.Pkg(), m.Name(), methodCapabilityType(m.Type()).(*types.Signature))
		}
		embeddeds := make([]types.Type, t.NumEmbeddeds())
		for i := range embeddeds {
			embeddeds[i] = methodCapabilityType(t.EmbeddedType(i))
		}
		return types.NewInterfaceType(methods, embeddeds).Complete()
	case *types.Map:
		return types.NewMap(methodCapabilityType(t.Key()), methodCapabilityType(t.Elem()))
	case *types.Chan:
		return types.NewChan(t.Dir(), methodCapabilityType(t.Elem()))
	case *types.Named:
		targs := t.TypeArgs()
		if targs == nil || targs.Len() == 0 {
			return t
		}
		args := make([]types.Type, targs.Len())
		for i := range args {
			args[i] = methodCapabilityType(targs.At(i))
		}
		inst, err := types.Instantiate(types.NewContext(), t.Origin(), args, false)
		if err != nil {
			return t
		}
		return inst
	case *types.Union:
		terms := make([]*types.Term, t.Len())
		for i := range terms {
			term := t.Term(i)
			terms[i] = types.NewTerm(term.Tilde(), methodCapabilityType(term.Type()))
		}
		return types.NewUnion(terms)
	default:
		return t
	}
}

func methodCapabilityKey(method *types.Func) string {
	return "go.method." + method.Name() + ":" + methodCapabilitySig(method.Type().(*types.Signature))
}

func (p Program) llvmTypeCheckedLoad(mod llvm.Module) llvm.Value {
	fn := mod.NamedFunction("llvm.type.checked.load")
	if !fn.IsNil() {
		return fn
	}
	mdTy := p.ctx.MetadataType()
	retTy := p.ctx.StructType([]llvm.Type{p.tyVoidPtr(), p.tyInt1()}, false)
	fnTy := llvm.FunctionType(retTy, []llvm.Type{p.tyVoidPtr(), p.Int32().ll, mdTy}, false)
	return llvm.AddFunction(mod, "llvm.type.checked.load", fnTy)
}

func (p Program) llvmAssume(mod llvm.Module) llvm.Value {
	fn := mod.NamedFunction("llvm.assume")
	if !fn.IsNil() {
		return fn
	}
	fnTy := llvm.FunctionType(p.tyVoid(), []llvm.Type{p.tyInt1()}, false)
	return llvm.AddFunction(mod, "llvm.assume", fnTy)
}

func (p Program) llvmFakeUse(mod llvm.Module) llvm.Value {
	fn := mod.NamedFunction("llvm.fake.use")
	if !fn.IsNil() {
		return fn
	}
	fnTy := llvm.FunctionType(p.tyVoid(), nil, true)
	return llvm.AddFunction(mod, "llvm.fake.use", fnTy)
}

func (p Program) addModuleFlag(mod llvm.Module, behavior uint64, name string, val uint64) {
	mod.AddNamedMetadataOperand("llvm.module.flags",
		p.ctx.MDNode([]llvm.Metadata{
			llvm.ConstInt(p.Int32().ll, behavior, false).ConstantAsMetadata(),
			p.ctx.MDString(name),
			llvm.ConstInt(p.Int32().ll, val, false).ConstantAsMetadata(),
		}),
	)
}

func (p Program) addVirtualFunctionElimModuleFlag(mod llvm.Module, enabled bool) {
	val := uint64(0)
	if enabled {
		val = 1
	}
	p.addModuleFlag(mod, 1, "Virtual Function Elim", val)
}

func (p Program) addTypeMetadata(global llvm.Value, offset uint64, typeID string) {
	kind := p.ctx.MDKindID("type")
	node := p.ctx.MDNode([]llvm.Metadata{
		llvm.ConstInt(p.Int64().ll, offset, false).ConstantAsMetadata(),
		p.ctx.MDString(typeID),
	})
	global.AddMetadata(kind, node)
}

func (p Program) setVCallVisibilityMetadata(global llvm.Value, vis uint64) {
	kind := p.ctx.MDKindID("vcall_visibility")
	node := p.ctx.MDNode([]llvm.Metadata{
		llvm.ConstInt(p.Int64().ll, vis, false).ConstantAsMetadata(),
	})
	global.AddMetadata(kind, node)
}

func (p Program) methodCheckedLoad(b llvm.Builder, mod llvm.Module, typedesc llvm.Value, typeID string) llvm.Value {
	mdVal := p.ctx.MetadataAsValue(p.ctx.MDString(typeID))
	res := llvm.CreateCall(b, p.llvmTypeCheckedLoad(mod).GlobalValueType(), p.llvmTypeCheckedLoad(mod), []llvm.Value{
		typedesc,
		llvm.ConstInt(p.Int32().ll, 0, false),
		mdVal,
	})
	ok := llvm.CreateExtractValue(b, res, 1)
	llvm.CreateCall(b, p.llvmAssume(mod).GlobalValueType(), p.llvmAssume(mod), []llvm.Value{ok})
	return llvm.CreateExtractValue(b, res, 0)
}

func (p Program) fakeUseValueInlineAsm(b llvm.Builder, v llvm.Value) {
	fnTy := llvm.FunctionType(p.tyVoid(), []llvm.Type{v.Type()}, false)
	asm := llvm.InlineAsm(fnTy, "", "X", true, false, llvm.InlineAsmDialectATT, false)
	llvm.CreateCall(b, fnTy, asm, []llvm.Value{v})
}

func (fn Function) emitFakeUses(b Builder) {
	if len(fn.fakeUses) == 0 || len(fn.blks) == 0 {
		return
	}
	curBlk := b.blk
	curInsert := b.impl.GetInsertBlock()
	b.SetBlockEx(fn.blks[0], AtStart, false)
	llvm.CreateCall(b.impl, fn.Prog.llvmFakeUse(fn.Pkg.Module()).GlobalValueType(), fn.Prog.llvmFakeUse(fn.Pkg.Module()), fn.fakeUses)
	if !curInsert.IsNil() {
		b.impl.SetInsertPointAtEnd(curInsert)
	}
	b.blk = curBlk
}

func (fn Function) emitFakeUsesInlineAsm(b Builder) {
	if len(fn.fakeUses) == 0 || len(fn.blks) == 0 {
		return
	}
	curBlk := b.blk
	curInsert := b.impl.GetInsertBlock()
	b.SetBlockEx(fn.blks[0], AtStart, false)
	for _, v := range fn.fakeUses {
		fn.Prog.fakeUseValueInlineAsm(b.impl, v)
	}
	if !curInsert.IsNil() {
		b.impl.SetInsertPointAtEnd(curInsert)
	}
	b.blk = curBlk
}

func (p Program) addMethodTypeMetadata(global llvm.Value, fullType Type, mset *types.MethodSet, methodCount int) {
	if !p.enableGoGlobalDCE {
		return
	}
	if methodCount == 0 {
		return
	}
	p.setVCallVisibilityMetadata(global, vcallVisibilityLinkageUnit)
	mt := p.rtNamed("Method")
	methodArrayOffset := p.OffsetOf(fullType, 2)
	methodType := p.Type(mt, InGo)
	ifnOffset := p.OffsetOf(methodType, 2)
	methodStride := p.SizeOf(methodType)
	for i := 0; i < methodCount; i++ {
		sel := mset.At(i)
		offset := methodArrayOffset + uint64(i)*methodStride + ifnOffset
		p.addTypeMetadata(global, offset, methodCapabilityKey(sel.Obj().(*types.Func)))
		if sel.Obj().Exported() {
			p.addTypeMetadata(global, offset, "go.method.reflect")
		}
	}
}
