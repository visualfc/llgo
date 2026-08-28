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
	_ = p.stdcallCallConv()
	if sig.Variadic() || HasNameValist(sig) {
		panic("stdcall does not support variadic functions; use the ordinary C ABI")
	}
}

func (p Program) validateStdcallType(typ types.Type) {
	sig, ok := types.Unalias(typ).Underlying().(*types.Signature)
	if !ok {
		panic(fmt.Errorf("stdcall requires a function type, got %s", typ))
	}
	p.validateStdcallSignature(sig)
}

func (p Program) isStdcallType(typ types.Type) bool {
	named, ok := types.Unalias(typ).(*types.Named)
	if !ok {
		return false
	}
	background, ok := p.packageTypeBackground(namedLinkname(named))
	return ok && background == InStdcall
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

// stdcallCallback adapts a static Go entry to the native stdcall boundary.
// It deliberately rejects closure values: the native representation is one
// function pointer and has no implicit slot for an LLGo closure environment.
func (b Builder) stdcallCallback(typ types.Type, fn Expr) Expr {
	dst := b.Prog.Type(typ, InStdcall)
	expected := b.Prog.stdcallCallConv()
	direct := fn.impl.IsAFunction()
	if direct.IsNil() {
		panic("stdcall callback must be a non-capturing function")
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
