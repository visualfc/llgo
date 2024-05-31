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

package runtime

import (
	"unsafe"

	"github.com/goplus/llgo/internal/abi"
)

type Kind = abi.Kind
type Type = abi.Type

// -----------------------------------------------------------------------------

func Basic(kind Kind) *Type {
	return basicTypes[kind]
}

var (
	basicTypes = [...]*Type{
		abi.Bool:       basicType(abi.Bool),
		abi.Int:        basicType(abi.Int),
		abi.Int8:       basicType(abi.Int8),
		abi.Int16:      basicType(abi.Int16),
		abi.Int32:      basicType(abi.Int32),
		abi.Int64:      basicType(abi.Int64),
		abi.Uint:       basicType(abi.Uint),
		abi.Uint8:      basicType(abi.Uint8),
		abi.Uint16:     basicType(abi.Uint16),
		abi.Uint32:     basicType(abi.Uint32),
		abi.Uint64:     basicType(abi.Uint64),
		abi.Uintptr:    basicType(abi.Uintptr),
		abi.Float32:    basicType(abi.Float32),
		abi.Float64:    basicType(abi.Float64),
		abi.Complex64:  basicType(abi.Complex64),
		abi.Complex128: basicType(abi.Complex128),
		abi.String:     basicType(abi.String),
	}
)

var (
	sizeBasicTypes = [...]uintptr{
		abi.Bool:       unsafe.Sizeof(false),
		abi.Int:        unsafe.Sizeof(0),
		abi.Int8:       1,
		abi.Int16:      2,
		abi.Int32:      4,
		abi.Int64:      8,
		abi.Uint:       unsafe.Sizeof(uint(0)),
		abi.Uint8:      1,
		abi.Uint16:     2,
		abi.Uint32:     4,
		abi.Uint64:     8,
		abi.Uintptr:    unsafe.Sizeof(uintptr(0)),
		abi.Float32:    4,
		abi.Float64:    8,
		abi.Complex64:  8,
		abi.Complex128: 16,
		abi.String:     unsafe.Sizeof(String{}),
	}
	alignBasicTypes = [...]uintptr{
		abi.Bool:       unsafe.Alignof(false),
		abi.Int:        unsafe.Alignof(0),
		abi.Int8:       1,
		abi.Int16:      2,
		abi.Int32:      4,
		abi.Int64:      8,
		abi.Uint:       unsafe.Alignof(uint(0)),
		abi.Uint8:      1,
		abi.Uint16:     2,
		abi.Uint32:     4,
		abi.Uint64:     8,
		abi.Uintptr:    unsafe.Alignof(uintptr(0)),
		abi.Float32:    4,
		abi.Float64:    8,
		abi.Complex64:  8,
		abi.Complex128: 16,
		abi.String:     unsafe.Alignof(""),
	}
)

const pointerAlign = unsafe.Alignof(uintptr(0))

func basicType(kind abi.Kind) *Type {
	align := alignBasicTypes[kind]
	return &Type{
		Size_:       sizeBasicTypes[kind],
		Hash:        uint32(kind), // TODO(xsw): hash
		Kind_:       uint8(kind),
		Align_:      uint8(align),
		FieldAlign_: uint8(align),
	}
}

// -----------------------------------------------------------------------------

// StructField returns a struct field.
func StructField(name string, typ *Type, off uintptr, tag string, embedded bool) abi.StructField {
	return abi.StructField{
		Name_:     name,
		Typ:       typ,
		Offset:    off,
		Tag_:      tag,
		Embedded_: embedded,
	}
}

// Struct returns a struct type.
func Struct(pkgPath string, size uintptr, fields ...abi.StructField) *Type {
	ret := &abi.StructType{
		Type: Type{
			Size_: size,
			Hash:  uint32(abi.Struct), // TODO(xsw): hash
			Kind_: uint8(abi.Struct),
		},
		PkgPath_: pkgPath,
		Fields:   fields,
	}
	var typalign uint8
	for _, f := range fields {
		ft := f.Typ
		if ft.Align_ > typalign {
			typalign = ft.Align_
		}
	}
	ret.Align_ = typalign
	ret.FieldAlign_ = typalign
	return &ret.Type
}

type fieldInfo struct {
	size  uintptr
	align uintptr
}

func structInfo(fields ...fieldInfo) (size uintptr, typalign uintptr, offsets []uintptr) {
	offsets = make([]uintptr, len(fields))
	for i, ft := range fields {
		offset := align(size, ft.align)
		if ft.align > typalign {
			typalign = ft.align
		}
		size = offset + ft.size
		offsets[i] = offset
	}
	return
}

func align(x, n uintptr) uintptr {
	return (x + n - 1) &^ (n - 1)
}

// -----------------------------------------------------------------------------

// PointerTo returns the pointer type with element elem.
func PointerTo(elem *Type) *Type {
	ret := elem.PtrToThis_
	if ret == nil {
		ret = newPointer(elem)
		elem.PtrToThis_ = ret
	}
	return ret
}

func newPointer(elem *Type) *Type {
	ptr := &abi.PtrType{
		Type: Type{
			Size_:       unsafe.Sizeof(uintptr(0)),
			Hash:        uint32(abi.Pointer), // TODO(xsw): hash
			Kind_:       uint8(abi.Pointer),
			Align_:      uint8(pointerAlign),
			FieldAlign_: uint8(pointerAlign),
		},
		Elem: elem,
	}
	return &ptr.Type
}

// SliceOf returns the slice type with element elem.
func SliceOf(elem *Type) *Type {
	ret := &abi.SliceType{
		Type: Type{
			Size_:       unsafe.Sizeof([]int{}),
			Hash:        uint32(abi.Slice),
			Kind_:       uint8(abi.Slice),
			Align_:      uint8(pointerAlign),
			FieldAlign_: uint8(pointerAlign),
		},
		Elem: elem,
	}
	return &ret.Type
}

// ArrayOf returns the array type with element elem and length.
func ArrayOf(length uintptr, elem *Type) *Type {
	ret := &abi.ArrayType{
		Type: Type{
			Size_:       length * elem.Size_,
			Hash:        uint32(abi.Array),
			Kind_:       uint8(abi.Array),
			Align_:      elem.Align_,
			FieldAlign_: elem.FieldAlign_,
		},
		Elem:  elem,
		Slice: SliceOf(elem),
		Len:   length,
	}
	return &ret.Type
}

// -----------------------------------------------------------------------------
