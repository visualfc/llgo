package ssa

import (
	"go/types"
	"strings"

	"github.com/xgo-dev/llvm"
)

const mapsRuntimePackage = "github.com/goplus/llgo/runtime/internal/runtime/maps."

func (b Builder) callAMD64MapIntrinsic(fn Expr, sig *types.Signature, args []Expr) (Expr, bool) {
	if b.Prog.target.GOARCH != "amd64" || !strings.HasPrefix(fn.Name(), mapsRuntimePackage) {
		return Expr{}, false
	}

	name := strings.TrimPrefix(fn.Name(), mapsRuntimePackage)
	retType := b.Prog.retType(sig)
	ret := func(v llvm.Value) (Expr, bool) { return Expr{v, retType}, true }

	switch name {
	case "bitsetFirst":
		return ret(b.impl.CreateIntrinsic(args[0].impl.Type(), llvm.LookupIntrinsicID("llvm.cttz"), []llvm.Value{
			args[0].impl, llvm.ConstInt(b.Prog.ctx.Int1Type(), 0, false),
		}, ""))
	case "bitsetRemoveBelow":
		one := llvm.ConstInt(args[0].impl.Type(), 1, false)
		mask := b.impl.CreateSub(b.impl.CreateShl(one, args[1].impl, ""), one, "")
		return ret(b.impl.CreateAnd(args[0].impl, b.impl.CreateNot(mask, ""), ""))
	case "bitsetLowestSet":
		one := llvm.ConstInt(args[0].impl.Type(), 1, false)
		low := b.impl.CreateAnd(args[0].impl, one, "")
		return ret(b.impl.CreateICmp(llvm.IntEQ, low, one, ""))
	case "bitsetShiftOutLowest":
		one := llvm.ConstInt(args[0].impl.Type(), 1, false)
		return ret(b.impl.CreateLShr(args[0].impl, one, ""))
	case "ctrlGroupMatchH2":
		group := mapGroupBytes(b, args[0].impl)
		h := b.impl.CreateTrunc(args[1].impl, b.Prog.ctx.Int8Type(), "")
		return ret(mapByteMask(b, b.impl.CreateICmp(llvm.IntEQ, group, splatByte(b, h), ""), retType.ll))
	case "ctrlGroupMatchEmpty":
		group := mapGroupBytes(b, args[0].impl)
		empty := llvm.ConstInt(b.Prog.ctx.Int8Type(), 0x80, false)
		return ret(mapByteMask(b, b.impl.CreateICmp(llvm.IntEQ, group, splatByte(b, empty), ""), retType.ll))
	case "ctrlGroupMatchEmptyOrDeleted":
		group := mapGroupBytes(b, args[0].impl)
		zero := llvm.ConstNull(group.Type())
		return ret(mapByteMask(b, b.impl.CreateICmp(llvm.IntSLT, group, zero, ""), retType.ll))
	case "ctrlGroupMatchFull":
		group := mapGroupBytes(b, args[0].impl)
		zero := llvm.ConstNull(group.Type())
		return ret(mapByteMask(b, b.impl.CreateICmp(llvm.IntSGE, group, zero, ""), retType.ll))
	default:
		return Expr{}, false
	}
}

func mapGroupBytes(b Builder, group llvm.Value) llvm.Value {
	return b.impl.CreateBitCast(group, llvm.VectorType(b.Prog.ctx.Int8Type(), 8), "")
}

func splatByte(b Builder, value llvm.Value) llvm.Value {
	vecType := llvm.VectorType(b.Prog.ctx.Int8Type(), 8)
	vec := b.impl.CreateInsertElement(llvm.Undef(vecType), value, llvm.ConstInt(b.Prog.ctx.Int32Type(), 0, false), "")
	indices := make([]llvm.Value, 8)
	for i := range indices {
		indices[i] = llvm.ConstInt(b.Prog.ctx.Int32Type(), 0, false)
	}
	return b.impl.CreateShuffleVector(vec, llvm.Undef(vecType), llvm.ConstVector(indices, false), "")
}

func mapByteMask(b Builder, matches llvm.Value, retType llvm.Type) llvm.Value {
	return b.impl.CreateZExt(b.impl.CreateBitCast(matches, b.Prog.ctx.Int8Type(), ""), retType, "")
}
