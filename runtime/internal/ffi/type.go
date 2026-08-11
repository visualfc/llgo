package ffi

import (
	"unsafe"

	"github.com/goplus/llgo/runtime/abi"
	c "github.com/goplus/llgo/runtime/internal/clite"
	"github.com/goplus/llgo/runtime/internal/clite/ffi"
)

type BasicKind int

const (
	Void BasicKind = iota // type is invalid

	// predeclared types
	Bool
	Int
	Int8
	Int16
	Int32
	Int64
	Uint
	Uint8
	Uint16
	Uint32
	Uint64
	Uintptr
	Float32
	Float64
	Complex64
	Complex128
	String
	UnsafePointer
	Interface
	Slice

	// aliases
	Byte = Uint8
	Rune = Int32
)

const (
	_64bit   = 1 << (^uintptr(0) >> 63) / 2
	_Int     = _64bit*ffi.Sint64 + (1-_64bit)*ffi.Sint32
	_Uint    = _64bit*ffi.Uint64 + (1-_64bit)*ffi.Uint32
	_sizei   = unsafe.Sizeof(0)
	_aligni  = uint16(unsafe.Alignof(0))
	_sizeci  = unsafe.Sizeof(c.Int(0))
	_alignci = uint16(unsafe.Alignof(c.Int(0)))
	_sizes   = unsafe.Sizeof("")
	_aligns  = uint16(unsafe.Alignof(""))
)

var (
	TypeVoid       = &Type{1, 1, ffi.Void, nil}
	TypeBool       = &Type{1, 1, ffi.Uint8, nil}
	TypeInt8       = &Type{1, 1, ffi.Sint8, nil}
	TypeInt16      = &Type{2, 2, ffi.Sint16, nil}
	TypeInt32      = &Type{4, 4, ffi.Sint32, nil}
	TypeInt64      = &Type{8, 8, ffi.Sint64, nil}
	TypeUint8      = &Type{1, 1, ffi.Uint8, nil}
	TypeUint16     = &Type{2, 2, ffi.Uint16, nil}
	TypeUint32     = &Type{4, 4, ffi.Uint32, nil}
	TypeUint64     = &Type{8, 8, ffi.Uint64, nil}
	TypeFloat32    = &Type{4, 4, ffi.Float, nil}
	TypeFloat64    = &Type{8, 8, ffi.Double, nil}
	TypeComplex64  = &Type{8, 4, ffi.Complex, &[]*Type{TypeFloat32, nil}[0]}
	TypeComplex128 = &Type{16, 8, ffi.Complex, &[]*Type{TypeFloat64, nil}[0]}
	TypeInt        = &Type{_sizei, _aligni, _Int, nil}
	TypeUint       = &Type{_sizei, _aligni, _Uint, nil}
	TypeUintptr    = &Type{_sizei, _aligni, _Uint, nil}
	TypePointer    = &Type{_sizei, _aligni, ffi.Pointer, nil}
	TypeString     = StructOf(TypePointer, TypeInt)
	TypeInterface  = StructOf(TypePointer, TypePointer)
	TypeSlice      = StructOf(TypePointer, TypeInt, TypeInt)
	empty          = [2]*Type{TypeInt8, nil}
	typeEmpty      = &Type{0, 0, ffi.Struct, &empty[0]}
	typeClosure    = StructOf(TypePointer, TypePointer)
)

var Typ = []*Type{
	Void:          TypeVoid,
	Bool:          TypeBool,
	Int:           TypeInt,
	Int8:          TypeInt8,
	Int16:         TypeInt16,
	Int32:         TypeInt32,
	Int64:         TypeInt64,
	Uint:          TypeUint,
	Uint8:         TypeUint8,
	Uint16:        TypeUint16,
	Uint32:        TypeUint32,
	Uint64:        TypeUint64,
	Uintptr:       TypeUintptr,
	Float32:       TypeFloat32,
	Float64:       TypeFloat64,
	Complex64:     TypeComplex64,
	Complex128:    TypeComplex128,
	String:        TypeString,
	UnsafePointer: TypePointer,
	Interface:     TypeInterface,
	Slice:         TypeSlice,
}

func ArrayOf(elem *Type, N int) *Type {
	if N == 0 {
		return typeEmpty
	}
	fs := make([]*Type, N+1)
	for i := 0; i < N; i++ {
		fs[i] = elem
	}
	return &Type{
		0,
		0,
		ffi.Struct,
		&fs[0],
	}
}

func StructOf(fields ...*Type) *Type {
	if len(fields) == 0 {
		return typeEmpty
	}
	fs := make([]*Type, len(fields)+1)
	copy(fs, fields)
	return &Type{
		0,
		0,
		ffi.Struct,
		&fs[0],
	}
}

// TypeOf returns the libffi representation of a Go ABI type.
func TypeOf(typ *abi.Type) *Type {
	switch kind := typ.Kind(); kind {
	case abi.Bool, abi.Int, abi.Int8, abi.Int16, abi.Int32, abi.Int64,
		abi.Uint, abi.Uint8, abi.Uint16, abi.Uint32, abi.Uint64, abi.Uintptr,
		abi.Float32, abi.Float64, abi.Complex64, abi.Complex128:
		return Typ[kind]
	case abi.Array:
		at := typ.ArrayType()
		return ArrayOf(TypeOf(at.Elem), int(at.Len))
	case abi.Chan, abi.Map, abi.Pointer, abi.UnsafePointer:
		return TypePointer
	case abi.Func:
		return typeClosure
	case abi.Interface:
		return TypeInterface
	case abi.Slice:
		return TypeSlice
	case abi.String:
		return TypeString
	case abi.Struct:
		if typ.IsClosure() {
			return typeClosure
		}
		return structTypeOf(typ)
	}
	panic("ffi.TypeOf: unsupported Go type " + typ.String())
}

func structTypeOf(typ *abi.Type) *Type {
	st := typ.StructType()
	fields := make([]*Type, 0, len(st.Fields))
	var off uintptr
	for _, field := range st.Fields {
		if field.Offset > off {
			fields, off = appendPadding(fields, off, field.Offset-off)
		}
		if field.Typ.Size_ == 0 {
			continue
		}
		fields = append(fields, TypeOf(field.Typ))
		off = field.Offset + field.Typ.Size_
	}
	// Do not pad to typ.Size_: trailing zero-sized fields can enlarge the
	// Go-visible size without consuming registers in llgo's callable ABI.
	return StructOf(fields...)
}

func appendPadding(fields []*Type, off, size uintptr) ([]*Type, uintptr) {
	for size > 0 {
		switch {
		case off%8 == 0 && size >= 8:
			fields = append(fields, TypeUint64)
			off += 8
			size -= 8
		case off%4 == 0 && size >= 4:
			fields = append(fields, TypeUint32)
			off += 4
			size -= 4
		case off%2 == 0 && size >= 2:
			fields = append(fields, TypeUint16)
			off += 2
			size -= 2
		default:
			fields = append(fields, TypeUint8)
			off++
			size--
		}
	}
	return fields, off
}

// ReturnTypeOf returns the libffi representation of a Go function's results.
func ReturnTypeOf(results []*abi.Type) *Type {
	switch len(results) {
	case 0:
		return TypeVoid
	case 1:
		return TypeOf(results[0])
	default:
		fields := make([]*Type, len(results))
		for i, result := range results {
			fields[i] = TypeOf(result)
		}
		return StructOf(fields...)
	}
}

// NewGoSignature prepares a libffi signature for Go ABI parameter and result
// types.
func NewGoSignature(params, results []*abi.Type) (*Signature, error) {
	args := make([]*Type, len(params))
	for i, param := range params {
		args[i] = TypeOf(param)
	}
	return NewSignature(ReturnTypeOf(results), args...)
}
