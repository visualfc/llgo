//go:build !dev
// +build !dev

package ssa

import (
	"go/types"

	"github.com/xgo-dev/llvm"
)

const (
	goGlobalDCEAvailable        = false
	vcallVisibilityLinkageUnit  = 1
	LLVMModuleFlagBehaviorError = 1
)

func methodCapabilitySig(sig *types.Signature) string {
	return ""
}

func methodCapabilityTuple(tuple *types.Tuple) *types.Tuple {
	return nil
}

func methodCapabilityType(t types.Type) types.Type {
	return t
}

func methodCapabilityKey(method *types.Func) string {
	return ""
}

func (p Program) llvmTypeCheckedLoad(mod llvm.Module) llvm.Value {
	return llvm.Value{}
}

func (p Program) llvmAssume(mod llvm.Module) llvm.Value {
	return llvm.Value{}
}

func (p Program) llvmFakeUse(mod llvm.Module) llvm.Value {
	return llvm.Value{}
}

func (p Program) addModuleFlag(mod llvm.Module, behavior uint64, name string, val uint64) {
}

func (p Program) addVirtualFunctionElimModuleFlag(mod llvm.Module) {
}

func (p Program) addTypeMetadata(global llvm.Value, offset uint64, typeID string) {
}

func (p Program) setVCallVisibilityMetadata(global llvm.Value, vis uint64) {
}

func (p Program) methodCheckedLoad(b llvm.Builder, mod llvm.Module, typedesc llvm.Value, typeID string) llvm.Value {
	return llvm.Value{}
}

func (p Program) fakeUseValueInlineAsm(b llvm.Builder, v llvm.Value) {
}

func (fn Function) emitFakeUses(b Builder) {
}

func (fn Function) emitFakeUsesInlineAsm(b Builder) {
}

func (p Function) recordFakeUse(v llvm.Value) {
}

func (p Program) addMethodTypeMetadata(global llvm.Value, fullType Type, mset *types.MethodSet, methodCount int) {
}

func peelConstOperand0ToType(v llvm.Value, target llvm.Type) llvm.Value {
	return v
}

func (b Builder) recordAbiTypeFakeUses(t types.Type, global llvm.Value) {
}
