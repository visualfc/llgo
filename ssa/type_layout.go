/*
 * Copyright (c) 2024 The XGo Authors (xgo.dev). All rights reserved.
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

	"github.com/xgo-dev/llvm"
)

type structLayout struct {
	wrapped []bool
}

func (p Program) hasNativeStructLayout(raw *types.Named) bool {
	background, ok := p.packageTypeBackground(namedLinkname(raw))
	return ok && isNativeFuncBackground(background)
}

func (p Program) setStructLayout(typ types.Type, layout *structLayout) {
	if layout != nil {
		p.structLayouts.Set(typ, *layout)
	}
}

func (p Program) structLayout(typ Type) (structLayout, bool) {
	layout := p.structLayouts.At(typ.raw.Type)
	if layout == nil {
		return structLayout{}, false
	}
	return layout.(structLayout), true
}

// toLLVMStructBody preserves the native target layout for C types. On 386,
// LLVM's target layout aligns i64 and double fields to eight bytes while Go
// aligns them to four. For Go structs whose offsets or size differ, it wraps
// only the affected fields and keeps the Go field index as the outer LLVM
// element index.
func (p Program) toLLVMStructBody(raw *types.Struct, native bool) ([]llvm.Type, *structLayout) {
	fields := p.toLLVMFields(raw)
	if native || p.target.effectiveGOARCH() != "386" || len(fields) == 0 {
		return fields, nil
	}

	sizes := p.sizes
	if sizes == nil {
		sizes = types.SizesFor("gc", "386")
		p.sizes = sizes
	}
	offsets := make([]int64, len(fields))
	alignments := make([]int64, len(fields))
	goAlign := int64(1)
	goSize := int64(0)
	for i, field := range fields {
		fieldAlign := sizes.Alignof(raw.Field(i).Type())
		if p.hasNativeTypeLayout(raw.Field(i).Type()) {
			fieldAlign = int64(p.td.ABITypeAlignment(field))
		}
		alignments[i] = fieldAlign
		if fieldAlign > goAlign {
			goAlign = fieldAlign
		}
		goSize = align(goSize, fieldAlign)
		offsets[i] = goSize
		fieldSize := int64(p.td.TypeAllocSize(field))
		if i == len(fields)-1 && goSize > 0 && fieldSize == 0 {
			// Match cmd/compile: a final zero-sized field must have an
			// address inside the enclosing non-zero-sized object.
			fieldSize = 1
		}
		goSize += fieldSize
	}
	goSize = align(goSize, goAlign)

	nativeType := p.ctx.StructType(fields, false)
	if int64(p.td.TypeAllocSize(nativeType)) == goSize &&
		int64(p.td.ABITypeAlignment(nativeType)) == goAlign {
		equal := true
		for i, offset := range offsets {
			if int64(p.td.ElementOffset(nativeType, i)) != offset {
				equal = false
				break
			}
		}
		if equal {
			return fields, nil
		}
	}

	wrapped := make([]bool, len(fields))
	body := make([]llvm.Type, 0, len(fields)+1)
	for i, field := range fields {
		end := goSize
		if i+1 < len(offsets) {
			end = offsets[i+1]
		}
		fieldSize := int64(p.td.TypeAllocSize(field))
		padding := end - offsets[i] - fieldSize
		fieldAlign := int64(p.td.ABITypeAlignment(field))
		goFieldAlign := alignments[i]
		if padding < 0 {
			panic(fmt.Sprintf("Go 386 field layout is smaller than its LLVM representation: %s field %d", raw, i))
		}
		if padding != 0 || fieldAlign > goFieldAlign {
			parts := []llvm.Type{field}
			if padding != 0 {
				parts = append(parts, llvm.ArrayType(p.tyInt8(), int(padding)))
			}
			field = p.ctx.StructType(parts, true)
			wrapped[i] = true
		}
		body = append(body, field)
	}

	probe := p.ctx.StructType(body, false)
	if int64(p.td.ABITypeAlignment(probe)) < goAlign {
		// Go/386 alignments are powers of two no greater than four, and the
		// corresponding LLVM integer type has that same ABI alignment.
		alignType := p.ctx.IntType(int(goAlign * 8))
		body = append(body, llvm.ArrayType(alignType, 0))
		probe = p.ctx.StructType(body, false)
	}
	if int64(p.td.TypeAllocSize(probe)) != goSize {
		panic(fmt.Sprintf("invalid Go 386 LLVM struct size: got %d, want %d for %s", p.td.TypeAllocSize(probe), goSize, raw))
	}
	for i, offset := range offsets {
		if got := int64(p.td.ElementOffset(probe, i)); got != offset {
			panic(fmt.Sprintf("invalid Go 386 LLVM field offset: got %d, want %d for %s field %d", got, offset, raw, i))
		}
	}
	return body, &structLayout{wrapped: wrapped}
}

func (p Program) hasNativeTypeLayout(raw types.Type) bool {
	switch t := types.Unalias(raw).(type) {
	case *types.Named:
		return p.hasNativeStructLayout(t)
	case *types.Array:
		return p.hasNativeTypeLayout(t.Elem())
	}
	return false
}
