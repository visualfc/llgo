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
	"go/token"
	"go/types"
)

const (
	runtimeCurrentLocalContext = "currentLocalContext"
	runtimeLocalContext        = "LocalContext"
	runtimeLocalBlock          = "localBlock"
)

// EnterLocalContext creates the stack root used by TLS/GLS locality blocks and
// installs it for the current outermost Go entry. previous is nonzero only for
// a nested entry that inherited an existing context.
func (b Builder) EnterLocalContext() (ctx, previous Expr) {
	fn := b.Pkg.rtFunc("EnterLocalContext")
	params := fn.raw.Type.(*types.Signature).Params()
	ctxPtr := b.Prog.rawType(params.At(0).Type())
	ctxType := b.Prog.rawType(ctxPtr.RawType().(*types.Pointer).Elem())
	ctx = b.Alloc(ctxType, false)
	previous = b.Call(fn, ctx)
	return
}

// LeaveLocalContext restores an inherited context or drops the stack roots
// installed by EnterLocalContext.
func (b Builder) LeaveLocalContext(ctx, previous Expr) {
	b.Call(b.Pkg.rtFunc("LeaveLocalContext"), ctx, previous)
}

// BuildLocalPackageAccessor builds an always-hot package block lookup around
// LocalPackage. Runtime Go types and locality metadata are the ABI: they supply
// the context/header layouts and whether the context anchor is native TLS or an
// ordinary global. The generated code therefore does not duplicate target
// selection or byte offsets.
func (p Function) BuildLocalPackageAccessor(key, size, align Expr) {
	prog := p.Prog
	runtimePkg := prog.runtime()
	contextType := runtimePkg.Scope().Lookup(runtimeLocalContext).Type()
	blockType := runtimePkg.Scope().Lookup(runtimeLocalBlock).Type()
	_, contextBlocks, _ := types.LookupFieldOrMethod(contextType, true, runtimePkg, "blocks")
	_, blockKey, _ := types.LookupFieldOrMethod(blockType, true, runtimePkg, "key")

	b := p.MakeBody(5)
	checkHead := p.Block(1)
	checkKey := p.Block(2)
	hit := p.Block(3)
	slow := p.Block(4)
	key = b.Convert(prog.VoidPtr(), key)

	current := b.Load(p.Pkg.runtimeGlobal(runtimeCurrentLocalContext).Expr)
	hasContext := b.BinOp(token.NEQ, current, prog.IntVal(0, prog.Uintptr()))
	b.If(hasContext, checkHead, slow)

	b.SetBlock(checkHead)
	context := b.Convert(prog.Pointer(prog.Type(contextType, InGo)), current)
	head := b.Load(b.FieldAddr(context, contextBlocks[0]))
	hasHead := b.BinOp(token.NEQ, head, prog.Nil(head.Type))
	b.If(hasHead, checkKey, slow)

	b.SetBlock(checkKey)
	headerAddress := b.BinOp(
		token.SUB,
		b.Convert(prog.Uintptr(), head),
		prog.IntVal(prog.SizeOf(prog.Type(blockType, InGo)), prog.Uintptr()),
	)
	header := b.Convert(prog.Pointer(prog.Type(blockType, InGo)), headerAddress)
	foundKey := b.Load(b.FieldAddr(header, blockKey[0]))
	b.If(b.BinOp(token.EQL, foundKey, key), hit, slow)

	b.SetBlock(hit)
	result := p.raw.Type.(*types.Signature).Results().At(0).Type()
	b.Return(b.Convert(prog.rawType(result), head))

	b.SetBlock(slow)
	raw := b.Call(p.Pkg.rtFunc("LocalPackage"), key, size, align)
	b.Return(b.Convert(prog.rawType(result), raw))
	b.EndBuild()
}

func (p Package) runtimeGlobal(name string) Global {
	p.NeedRuntime = true
	runtimePkg := p.Prog.runtime()
	variable := runtimePkg.Scope().Lookup(name).(*types.Var)
	fullName := FullName(runtimePkg, name)
	typ := types.NewPointer(variable.Type())
	if locality, ok := p.Prog.VariableLocality(fullName); ok && locality.LocalStorage == LocalStorageNativeTLS {
		return p.NewThreadLocalVar(fullName, typ, InGo)
	}
	return p.NewVar(fullName, typ, InGo)
}
