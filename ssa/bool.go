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

import "github.com/xgo-dev/llvm"

func isLLVMInt1(t llvm.Type) bool {
	return t.TypeKind() == llvm.IntegerTypeKind && t.IntTypeWidth() == 1
}

// boolI1 converts a Go bool to an i1 for br, select, and other LLVM
// instructions that require a one-bit condition.
func (b Builder) boolI1(cond Expr) llvm.Value {
	v := cond.impl
	if isLLVMInt1(v.Type()) {
		return v
	}
	if !v.IsAZExtInst().IsNil() {
		src := v.Operand(0)
		if isLLVMInt1(src.Type()) {
			return src
		}
	}
	zero := llvm.ConstInt(v.Type(), 0, false)
	return llvm.CreateICmp(b.impl, llvm.IntNE, v, zero)
}

// boolFromI1 converts an LLVM i1 (typically icmp/fcmp) into a Go bool.
func (b Builder) boolFromI1(v llvm.Value) Expr {
	t := b.Prog.Bool()
	if isLLVMInt1(t.ll) || !isLLVMInt1(v.Type()) {
		return Expr{v, t}
	}
	return Expr{llvm.CreateZExt(b.impl, v, t.ll), t}
}

func (b Builder) boolNot(x Expr) Expr {
	t := b.Prog.Bool()
	if isLLVMInt1(x.impl.Type()) {
		return Expr{llvm.CreateNot(b.impl, x.impl), t}
	}
	return Expr{llvm.CreateXor(b.impl, x.impl, llvm.ConstInt(x.impl.Type(), 1, false)), t}
}

func (b Builder) coerceInt(v llvm.Value, dst llvm.Type) llvm.Value {
	if v.Type() == dst {
		return v
	}
	src := v.Type()
	if src.TypeKind() != llvm.IntegerTypeKind || dst.TypeKind() != llvm.IntegerTypeKind {
		return v
	}
	if isLLVMInt1(src) && dst.IntTypeWidth() == 8 {
		return llvm.CreateZExt(b.impl, v, dst)
	}
	if src.IntTypeWidth() == 8 && isLLVMInt1(dst) {
		return llvm.CreateICmp(b.impl, llvm.IntNE, v, llvm.ConstInt(src, 0, false))
	}
	return v
}

// coerceLLVM matches v's LLVM value to dst, inserting zext/trunc for bool.
func (b Builder) coerceLLVM(v Expr, dst Type) Expr {
	return Expr{b.coerceInt(v.impl, dst.ll), dst}
}

func (b Builder) boolAnd(x, y Expr) Expr {
	return Expr{b.impl.CreateAnd(x.impl, y.impl, ""), b.Prog.Bool()}
}

func (b Builder) boolOr(x, y Expr) Expr {
	return Expr{b.impl.CreateOr(x.impl, y.impl, ""), b.Prog.Bool()}
}
