/*
 * Copyright (c) 2024 The GoPlus Authors (goplus.org). All rights reserved.
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

	"github.com/goplus/llvm"
)

type valueKind = int

type Expr struct {
	impl llvm.Value
	Type
}

const (
	mathOpBase = token.ADD
	mathOpLast = token.REM
)

const (
	vkInvalid valueKind = iota
	vkSigned
	vkUnsigned
	vkFloat
	vkComplex
	vkString
	vkBool
	vkFunc
)

var mathOpToLLVM = []llvm.Opcode{
	int(token.ADD-mathOpBase)<<2 | vkSigned:   llvm.Add,
	int(token.ADD-mathOpBase)<<2 | vkUnsigned: llvm.Add,
	int(token.ADD-mathOpBase)<<2 | vkFloat:    llvm.FAdd,

	int(token.SUB-mathOpBase)<<2 | vkSigned:   llvm.Sub,
	int(token.SUB-mathOpBase)<<2 | vkUnsigned: llvm.Sub,
	int(token.SUB-mathOpBase)<<2 | vkFloat:    llvm.FSub,

	int(token.MUL-mathOpBase)<<2 | vkSigned:   llvm.Mul,
	int(token.MUL-mathOpBase)<<2 | vkUnsigned: llvm.Mul,
	int(token.MUL-mathOpBase)<<2 | vkFloat:    llvm.FMul,

	int(token.QUO-mathOpBase)<<2 | vkSigned:   llvm.SDiv,
	int(token.QUO-mathOpBase)<<2 | vkUnsigned: llvm.UDiv,
	int(token.QUO-mathOpBase)<<2 | vkFloat:    llvm.FDiv,

	int(token.REM-mathOpBase)<<2 | vkSigned:   llvm.SRem,
	int(token.REM-mathOpBase)<<2 | vkUnsigned: llvm.URem,
	int(token.REM-mathOpBase)<<2 | vkFloat:    llvm.FRem,
}

func mathOpIdx(op token.Token, x valueKind) int {
	return int(op-mathOpBase)<<2 | x
}

// ADD SUB MUL QUO REM          + - * / %
func isMathOp(op token.Token) bool {
	return op >= mathOpBase && op <= mathOpLast
}

const (
	logicOpBase = token.AND
	logicOpLast = token.AND_NOT
)

var logicOpToLLVM = []llvm.Opcode{
	token.AND - logicOpBase: llvm.And,
	token.OR - logicOpBase:  llvm.Or,
	token.XOR - logicOpBase: llvm.Xor,
	token.SHL - logicOpBase: llvm.Shl,
	token.SHR - logicOpBase: llvm.LShr,
}

// AND OR XOR SHL SHR AND_NOT   & | ^ << >> &^
func isLogicOp(op token.Token) bool {
	return op >= logicOpBase && op <= logicOpLast
}

const (
	predOpBase = token.EQL
	predOpLast = token.GEQ
)

var intPredOpToLLVM = []llvm.IntPredicate{
	token.EQL - predOpBase: llvm.IntEQ,
	token.NEQ - predOpBase: llvm.IntNE,
	token.LSS - predOpBase: llvm.IntSLT,
	token.LEQ - predOpBase: llvm.IntSLE,
	token.GTR - predOpBase: llvm.IntSGT,
	token.GEQ - predOpBase: llvm.IntSGE,
}

var uintPredOpToLLVM = []llvm.IntPredicate{
	token.EQL - predOpBase: llvm.IntEQ,
	token.NEQ - predOpBase: llvm.IntNE,
	token.LSS - predOpBase: llvm.IntULT,
	token.LEQ - predOpBase: llvm.IntULE,
	token.GTR - predOpBase: llvm.IntUGT,
	token.GEQ - predOpBase: llvm.IntUGE,
}

var floatPredOpToLLVM = []llvm.FloatPredicate{
	token.EQL - predOpBase: llvm.FloatOEQ,
	token.NEQ - predOpBase: llvm.FloatONE,
	token.LSS - predOpBase: llvm.FloatOLT,
	token.LEQ - predOpBase: llvm.FloatOLE,
	token.GTR - predOpBase: llvm.FloatOGT,
	token.GEQ - predOpBase: llvm.FloatOGE,
}

// EQL NEQ LSS LEQ GTR GEQ      == != < <= < >=
func isPredOp(op token.Token) bool {
	return op >= predOpBase && op <= predOpLast
}

// op:
// ADD SUB MUL QUO REM          + - * / %
// AND OR XOR SHL SHR AND_NOT   & | ^ << >> &^
// EQL NEQ LSS LEQ GTR GEQ      == != < <= < >=
func (b Builder) BinOp(op token.Token, x, y Expr) Expr {
	switch {
	case isMathOp(op): // op: + - * / %
		kind := x.kind
		switch kind {
		case vkString, vkComplex:
			panic("todo")
		}
		idx := mathOpIdx(op, kind)
		if llop := mathOpToLLVM[idx]; llop != 0 {
			return Expr{llvm.CreateBinOp(b.impl, llop, x.impl, y.impl), x.Type}
		}
	case isLogicOp(op): // op: & | ^ << >> &^
		if op == token.AND_NOT {
			panic("todo")
		}
		kind := x.kind
		llop := logicOpToLLVM[op-logicOpBase]
		if op == token.SHR && kind == vkUnsigned {
			llop = llvm.AShr
		}
		return Expr{llvm.CreateBinOp(b.impl, llop, x.impl, y.impl), x.Type}
	case isPredOp(op): // op: == != < <= < >=
		tret := b.prog.Bool()
		kind := x.kind
		switch kind {
		case vkSigned:
			pred := intPredOpToLLVM[op-predOpBase]
			return Expr{llvm.CreateICmp(b.impl, pred, x.impl, y.impl), tret}
		case vkUnsigned:
			pred := uintPredOpToLLVM[op-predOpBase]
			return Expr{llvm.CreateICmp(b.impl, pred, x.impl, y.impl), tret}
		case vkFloat:
			pred := floatPredOpToLLVM[op-predOpBase]
			return Expr{llvm.ConstFCmp(pred, x.impl, y.impl), tret}
		case vkString, vkComplex, vkBool:
			panic("todo")
		}
	}
	panic("todo")
}
