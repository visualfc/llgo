// Package dcepass rewrites ABI metadata so link-time dead code elimination can
// drop method bodies that are no longer referenced by live method slots.
package dcepass

import (
	"fmt"
	"io"
	"strings"

	"github.com/xgo-dev/llvm"
)

const unreachableMethodName = "github.com/goplus/llgo/runtime/internal/runtime.unreachableMethod"

// EmitStrongTypeOverrides emits method-pruned strong ABI type symbols into dst.
// srcMods contains the original package modules. For each constant ABI type
// global with a method array, it creates a same-name strong global in dst and
// clears IFn/TFn for method slots not listed in liveSlots[typeName].
func EmitStrongTypeOverrides(dst llvm.Module, srcMods []llvm.Module, liveSlots map[string][]int) error {
	return EmitStrongTypeOverridesDebug(dst, srcMods, liveSlots, nil)
}

// EmitStrongTypeOverridesDebug is EmitStrongTypeOverrides with one debug line
// per method slot whose IFn/TFn references are cleared when logw is non-nil.
func EmitStrongTypeOverridesDebug(dst llvm.Module, srcMods []llvm.Module, liveSlots map[string][]int, logw io.Writer) error {
	emitted := make(map[string]bool)
	emitter := newOverrideEmitter(dst)
	for _, src := range srcMods {
		for g := src.FirstGlobal(); !g.IsNil(); g = llvm.NextGlobal(g) {
			name := g.Name()
			if emitted[name] || !g.IsGlobalConstant() {
				continue
			}
			methodsVal, elemTy, ok := methodArray(g.Initializer())
			if !ok {
				continue
			}
			if err := emitter.emitTypeOverride(g, methodsVal, elemTy, liveSlotSet(liveSlots[name]), logw); err != nil {
				return fmt.Errorf("emit override %q: %w", name, err)
			}
			emitted[name] = true
		}
	}
	return nil
}

type overrideEmitter struct {
	dst    llvm.Module
	values map[llvm.Value]llvm.Value
}

func newOverrideEmitter(dst llvm.Module) *overrideEmitter {
	return &overrideEmitter{dst: dst, values: make(map[llvm.Value]llvm.Value)}
}

func (e *overrideEmitter) emitTypeOverride(srcType, methodsVal llvm.Value, elemTy llvm.Type, keepIdx map[int]bool, logw io.Writer) error {
	init := srcType.Initializer()
	dstType := e.ensureOverrideGlobal(srcType)
	e.values[srcType] = dstType

	fieldCount := init.OperandsCount()
	fields := make([]llvm.Value, fieldCount)
	for i := 0; i < fieldCount-1; i++ {
		clone, err := e.cloneConst(init.Operand(i))
		if err != nil {
			return err
		}
		fields[i] = clone
	}

	elemFields := elemTy.StructElementTypes()
	unreachableMethod := e.unreachableMethod(elemFields[2])
	methods := make([]llvm.Value, methodsVal.OperandsCount())
	for i := range methods {
		orig := methodsVal.Operand(i)
		if keepIdx[i] {
			clone, err := e.cloneConst(orig)
			if err != nil {
				return err
			}
			methods[i] = clone
			continue
		}
		if logw != nil {
			fmt.Fprintf(logw, "[dce] drop method %s[%d] ifn=%s tfn=%s\n", srcType.Name(), i, valueName(orig.Operand(2)), valueName(orig.Operand(3)))
		}
		name, err := e.cloneConst(orig.Operand(0))
		if err != nil {
			return err
		}
		mtype, err := e.cloneConst(orig.Operand(1))
		if err != nil {
			return err
		}
		methods[i] = llvm.ConstNamedStruct(elemTy, []llvm.Value{
			name,
			mtype,
			unreachableMethod,
			unreachableMethod,
		})
	}
	fields[fieldCount-1] = llvm.ConstArray(elemTy, methods)

	dstType.SetInitializer(constStructOfType(init.Type(), fields))
	dstType.SetGlobalConstant(true)
	dstType.SetLinkage(llvm.ExternalLinkage)
	copyGlobalAttrs(dstType, srcType)
	return nil
}

func (e *overrideEmitter) unreachableMethod(ptrTy llvm.Type) llvm.Value {
	fn := e.dst.NamedFunction(unreachableMethodName)
	if fn.IsNil() {
		fn = llvm.AddFunction(e.dst, unreachableMethodName,
			llvm.FunctionType(e.dst.Context().VoidType(), nil, false))
	}
	if fn.Type() == ptrTy {
		return fn
	}
	return llvm.ConstBitCast(fn, ptrTy)
}

func (e *overrideEmitter) ensureOverrideGlobal(src llvm.Value) llvm.Value {
	name := src.Name()
	dst := e.dst.NamedGlobal(name)
	if dst.IsNil() {
		dst = llvm.AddGlobal(e.dst, src.GlobalValueType(), name)
	}
	e.values[src] = dst
	return dst
}

func (e *overrideEmitter) cloneConst(v llvm.Value) (llvm.Value, error) {
	if mapped, ok := e.values[v]; ok {
		return mapped, nil
	}
	if gv := v.IsAGlobalValue(); !gv.IsNil() {
		clone, err := e.cloneGlobalValue(gv)
		if err != nil {
			return llvm.Value{}, err
		}
		e.values[v] = clone
		return clone, nil
	}
	if !v.IsAConstantStruct().IsNil() {
		ops, err := e.cloneOperands(v)
		if err != nil {
			return llvm.Value{}, err
		}
		clone := constStructOfType(v.Type(), ops)
		e.values[v] = clone
		return clone, nil
	}
	return v, nil
}

func (e *overrideEmitter) cloneOperands(v llvm.Value) ([]llvm.Value, error) {
	ops := make([]llvm.Value, v.OperandsCount())
	for i := range ops {
		clone, err := e.cloneConst(v.Operand(i))
		if err != nil {
			return nil, err
		}
		ops[i] = clone
	}
	return ops, nil
}

func (e *overrideEmitter) cloneGlobalValue(v llvm.Value) (llvm.Value, error) {
	if mapped, ok := e.values[v]; ok {
		return mapped, nil
	}
	if fn := v.IsAFunction(); !fn.IsNil() {
		dstFn := e.dst.NamedFunction(fn.Name())
		if dstFn.IsNil() {
			dstFn = llvm.AddFunction(e.dst, fn.Name(), fn.GlobalValueType())
		}
		e.values[v] = dstFn
		return dstFn, nil
	}
	if gv := v.IsAGlobalVariable(); !gv.IsNil() {
		clone, err := e.cloneGlobalVariable(gv)
		if err != nil {
			return llvm.Value{}, err
		}
		e.values[v] = clone
		return clone, nil
	}
	name := v.Name()
	if name == "" {
		return llvm.Value{}, fmt.Errorf("unsupported unnamed global reference")
	}
	dst := e.dst.NamedGlobal(name)
	if dst.IsNil() {
		dst = llvm.AddGlobal(e.dst, v.GlobalValueType(), name)
		dst.SetLinkage(llvm.ExternalLinkage)
	}
	e.values[v] = dst
	return dst, nil
}

func (e *overrideEmitter) cloneGlobalVariable(src llvm.Value) (llvm.Value, error) {
	if mapped, ok := e.values[src]; ok {
		return mapped, nil
	}
	name := src.Name()
	if name != "" && !isLocalLinkage(src.Linkage()) {
		dst := e.dst.NamedGlobal(name)
		if dst.IsNil() {
			dst = llvm.AddGlobal(e.dst, src.GlobalValueType(), name)
			dst.SetLinkage(llvm.ExternalLinkage)
		}
		e.values[src] = dst
		return dst, nil
	}

	dst := llvm.AddGlobal(e.dst, src.GlobalValueType(), "")
	e.values[src] = dst
	copyGlobalAttrs(dst, src)
	dst.SetLinkage(src.Linkage())
	dst.SetGlobalConstant(src.IsGlobalConstant())
	if init := src.Initializer(); !init.IsNil() {
		clone, err := e.cloneConst(init)
		if err != nil {
			return llvm.Value{}, err
		}
		dst.SetInitializer(clone)
	}
	return dst, nil
}

func methodArray(init llvm.Value) (llvm.Value, llvm.Type, bool) {
	if init.IsNil() || init.OperandsCount() == 0 {
		return llvm.Value{}, llvm.Type{}, false
	}
	methodsVal := init.Operand(init.OperandsCount() - 1)
	if methodsVal.Type().TypeKind() != llvm.ArrayTypeKind {
		return llvm.Value{}, llvm.Type{}, false
	}
	elemTy := methodsVal.Type().ElementType()
	if elemTy.TypeKind() != llvm.StructTypeKind || elemTy.StructElementTypesCount() != 4 {
		return llvm.Value{}, llvm.Type{}, false
	}
	if !strings.Contains(elemTy.StructName(), "runtime/abi.Method") {
		return llvm.Value{}, llvm.Type{}, false
	}
	return methodsVal, elemTy, true
}

func liveSlotSet(slots []int) map[int]bool {
	out := make(map[int]bool, len(slots))
	for _, slot := range slots {
		out[slot] = true
	}
	return out
}

func copyGlobalAttrs(dst, src llvm.Value) {
	dst.SetVisibility(src.Visibility())
	if sec := src.Section(); sec != "" {
		dst.SetSection(sec)
	}
	dst.SetThreadLocal(src.IsThreadLocal())
	if align := src.Alignment(); align > 0 {
		dst.SetAlignment(align)
	}
}

func isLocalLinkage(linkage llvm.Linkage) bool {
	return linkage == llvm.PrivateLinkage || linkage == llvm.InternalLinkage
}

func valueName(v llvm.Value) string {
	if !v.IsAGlobalValue().IsNil() {
		if name := v.Name(); name != "" {
			return name
		}
	}
	if !v.IsAConstantExpr().IsNil() && v.OperandsCount() > 0 {
		return valueName(v.Operand(0))
	}
	if !v.IsAConstantPointerNull().IsNil() || v.IsNull() {
		return "<nil>"
	}
	if name := v.Name(); name != "" {
		return name
	}
	return v.String()
}

func constStructOfType(typ llvm.Type, fields []llvm.Value) llvm.Value {
	if typ.StructName() != "" {
		return llvm.ConstNamedStruct(typ, fields)
	}
	return llvm.ConstStruct(fields, typ.IsStructPacked())
}
