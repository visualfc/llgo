/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ssa

import (
	"fmt"
	"go/token"
	"go/types"
	"strings"

	"github.com/xgo-dev/llvm"
)

func isNativeFuncBackground(background Background) bool {
	return background == InC || background == InStdcall
}

// stdcallCallConv follows the source-level __stdcall semantics used by the
// Windows SDK. It is a distinct convention on x86 only; Microsoft accepts and
// ignores it on x64 and ARM64, where the target's native C ABI is used.
func (p Program) stdcallCallConv() llvm.CallConv {
	target := p.Target()
	if target.effectiveGOOS() != "windows" {
		panic(fmt.Errorf("stdcall is only defined for Windows targets, got %s/%s",
			target.effectiveGOOS(), target.effectiveGOARCH()))
	}
	switch target.effectiveGOARCH() {
	case "386":
		return llvm.X86StdcallCallConv
	case "amd64", "arm64":
		return llvm.CCallConv
	default:
		panic(fmt.Errorf("stdcall is not supported on windows/%s", target.effectiveGOARCH()))
	}
}

// stdcallSymbolName suppresses LLVM's automatic 32-bit COFF decoration when a
// binding already names the decorated symbol explicitly (for example,
// _WindowProc@16). LLVM uses a leading byte 1 to mark such assembler names as
// literal. Undecorated names retain the normal x86 stdcall decoration, while
// x64 and ARM64 keep their unified native Windows symbol spelling.
func (p Program) stdcallSymbolName(name string) string {
	if p.stdcallCallConv() != llvm.X86StdcallCallConv || !isDecoratedStdcallSymbol(name) {
		return name
	}
	return "\x01" + name
}

func isDecoratedStdcallSymbol(name string) bool {
	if len(name) < 4 || name[0] != '_' {
		return false
	}
	at := strings.LastIndexByte(name, '@')
	if at <= 1 || at == len(name)-1 {
		return false
	}
	for _, ch := range name[at+1:] {
		if ch < '0' || ch > '9' {
			return false
		}
	}
	return true
}

func (p Program) validateStdcallSignature(sig *types.Signature) {
	if sig.Variadic() {
		panic(fmt.Errorf("stdcall does not support variadic functions; use the ordinary C ABI"))
	}
	_ = p.stdcallCallConv()
}

func (p Program) validateStdcallType(typ types.Type) {
	sig, ok := types.Unalias(typ).Underlying().(*types.Signature)
	if !ok {
		panic(fmt.Errorf("stdcall requires a function type, got %s", typ))
	}
	p.validateStdcallSignature(sig)
}

func (p Program) isStdcallType(typ types.Type) bool {
	background, ok := p.nativeFuncTypeBackground(typ)
	return ok && background == InStdcall
}

func (p Program) nativeFuncTypeBackground(typ types.Type) (Background, bool) {
	named, ok := types.Unalias(typ).(*types.Named)
	if !ok {
		return inUnknown, false
	}
	background, ok := p.packageTypeBackground(namedLinkname(named))
	return background, ok && isNativeFuncBackground(background)
}

func (b Builder) setNativeCallConv(call llvm.Value, fn Expr) {
	callConv := llvm.CCallConv
	if direct := fn.impl.IsAFunction(); !direct.IsNil() {
		callConv = direct.FunctionCallConv()
	} else if b.Prog.isStdcallType(fn.raw.Type) {
		callConv = b.Prog.stdcallCallConv()
	}
	call.SetInstructionCallConv(callConv)
}

// stdcallCallback adapts a direct Go entry to the native stdcall boundary. It
// deliberately rejects Go func values: the native representation is one
// function pointer and has no implicit slot for an LLGo closure environment.
func (b Builder) stdcallCallback(typ types.Type, fn Expr) Expr {
	dst := b.Prog.Type(typ, InStdcall)
	expected := b.Prog.stdcallCallConv()
	direct := fn.impl.IsAFunction()
	if direct.IsNil() {
		panic("stdcall callback must be a direct function reference")
	}
	if direct.FunctionCallConv() == expected {
		return Expr{fn.impl, dst}
	}

	sig := types.Unalias(typ).Underlying().(*types.Signature)
	name := b.Pkg.Path() + ".__llgo_stdcall$" + direct.Name()
	wrapper := b.Pkg.FuncOf(name)
	if wrapper == nil {
		wrapper = b.Pkg.NewFunc(name, sig, InStdcall)
		wrapper.impl.SetLinkage(llvm.InternalLinkage)
		body := wrapper.MakeBody(1)
		args := make([]Expr, sig.Params().Len())
		for i := range args {
			args[i] = wrapper.Param(i)
		}
		result := body.Call(fn, args...)
		switch n := sig.Results().Len(); n {
		case 0:
			body.Return()
		case 1:
			body.Return(result)
		default:
			results := make([]Expr, n)
			for i := range results {
				results[i] = body.Extract(result, i)
			}
			body.Return(results...)
		}
	}
	return Expr{wrapper.impl, dst}
}

// nativeFuncBackground reports the source ABI of a native function pointer.
// Named native function types carry that information in package syntax;
// direct declarations are registered when their LLVM symbols are created.
func (b Builder) nativeFuncBackground(fn Expr) (Background, bool) {
	if background, ok := b.Prog.nativeFuncTypeBackground(fn.raw.Type); ok {
		return background, true
	}
	direct := fn.impl.IsAFunction()
	if direct.IsNil() {
		return inUnknown, false
	}
	background, ok := b.Prog.nativeFuncBackgrounds[direct]
	return background, ok
}

// nativeFuncvalBackground reports whether a native function pointer needs an
// adapter before it can be represented by an ordinary Go func value. In
// CABI-only mode, indirect Go calls deliberately retain the Go ABI, so both C
// and stdcall pointers need a native-boundary adapter. Outside that mode only
// 32-bit stdcall differs from the ordinary funcval call convention.
func (b Builder) nativeFuncvalBackground(fn Expr) (Background, bool) {
	background, ok := b.nativeFuncBackground(fn)
	if !ok {
		return inUnknown, false
	}
	if b.Prog.Target().CABIOnly ||
		background == InStdcall && b.Prog.stdcallCallConv() == llvm.X86StdcallCallConv {
		return background, true
	}
	return inUnknown, false
}

// NativeCallAttribute marks an indirect call that crosses a native ABI
// boundary. ModeCFunc uses it to distinguish these calls from ordinary Go
// funcval calls, which must retain Go-level signatures.
const NativeCallAttribute = "llgo.native.call"

// nativeFuncval adapts a native function pointer to LLGo's ordinary Go
// funcval representation. The funcval context carries the native pointer
// directly; the Go entry retrieves it through the hidden closure-context
// register and performs the final call with the source ABI. This avoids an
// allocation while preserving nil: a nil native pointer produces a funcval
// with a nil code pointer.
func (b Builder) nativeFuncval(dst Type, fn Expr, background Background) Expr {
	closure, ok := types.Unalias(dst.raw.Type).Underlying().(*types.Struct)
	if !ok || !IsClosure(closure) {
		panic(fmt.Errorf("native funcval target must be a Go function, got %s", dst.raw.Type))
	}
	goSig := closure.Field(0).Type().(*types.Signature)
	nativeSig := types.Unalias(fn.raw.Type).Underlying().(*types.Signature)
	abiName := "c"
	callConv := llvm.CCallConv
	if background == InStdcall {
		abiName = "stdcall"
		callConv = b.Prog.stdcallCallConv()
	}
	wrapperName := b.Pkg.Path() + ".__llgo_" + abiName + "_funcval$" + b.Prog.abi.FuncName(goSig)
	wrapper := b.Pkg.FuncOf(wrapperName)
	if wrapper == nil {
		env := types.NewVar(token.NoPos, nil, "$env", types.Typ[types.UnsafePointer])
		wrapper = b.Pkg.NewEnvFunc(wrapperName, goSig, InGo, env, false)
		wrapper.impl.SetLinkage(llvm.InternalLinkage)
		body := wrapper.MakeBody(1)
		args := make([]Expr, goSig.Params().Len())
		for i := range args {
			args[i] = wrapper.Param(i)
		}
		native := Expr{wrapper.Env().impl, b.Prog.rawType(nativeSig)}
		result := body.Call(native, args...)
		result.impl.SetInstructionCallConv(callConv)
		result.impl.AddCallSiteAttribute(-1, b.Prog.ctx.CreateStringAttribute(NativeCallAttribute, "1"))
		switch n := goSig.Results().Len(); n {
		case 0:
			body.Return()
		case 1:
			body.Return(result)
		default:
			results := make([]Expr, n)
			for i := range results {
				results[i] = body.Extract(result, i)
			}
			body.Return(results...)
		}
	}

	isNil := llvm.CreateICmp(b.impl, llvm.IntEQ, fn.impl, llvm.ConstNull(fn.impl.Type()))
	code := llvm.CreateSelect(b.impl, isNil, llvm.ConstNull(wrapper.impl.Type()), wrapper.impl)
	return b.aggregateValue(dst, code, fn.impl)
}
