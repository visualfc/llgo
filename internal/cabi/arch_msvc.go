package cabi

import "github.com/xgo-dev/llvm"

// TypeInfoWindowsAmd64 implements the Microsoft x64 ABI aggregate rules.
// Unlike the SysV ABI, ordinary aggregates are never split across multiple
// registers. Aggregates of exactly 1, 2, 4, or 8 bytes use one integer
// register; all other non-empty aggregates are passed or returned indirectly.
type TypeInfoWindowsAmd64 struct {
	*Transformer
}

func (p *TypeInfoWindowsAmd64) SupportByVal() bool {
	return false
}

func (p *TypeInfoWindowsAmd64) SkipEmptyParams() bool {
	return false
}

func (p *TypeInfoWindowsAmd64) IsWrapType(ctx llvm.Context, ftyp llvm.Type, typ llvm.Type, index int) bool {
	return isAggregateType(typ)
}

func (p *TypeInfoWindowsAmd64) GetTypeInfo(ctx llvm.Context, ftyp llvm.Type, typ llvm.Type, index int) *TypeInfo {
	info := &TypeInfo{Type: typ, Type1: typ}
	if typ.TypeKind() == llvm.VoidTypeKind {
		info.Kind = AttrVoid
		return info
	}
	if !isAggregateType(typ) {
		return info
	}

	info.Size = p.Sizeof(typ)
	info.Align = p.Alignof(typ)
	if info.Size == 0 {
		// Clang models Microsoft's empty-structure extension as a four-byte
		// integer argument/result on x64, even though LLVM's {} has size zero.
		info.Kind = AttrWidthType
		info.Type1 = ctx.Int32Type()
		return info
	}
	if isRegisterAggregateSize(info.Size) {
		info.Kind = AttrWidthType
		info.Type1 = ctx.IntType(info.Size * 8)
		return info
	}
	info.Kind = AttrPointer
	info.Type1 = llvm.PointerType(typ, 0)
	return info
}

// Windows on ARM64 follows the AAPCS64 aggregate classification implemented
// by TypeInfoArm64. Keep a distinct selected type so OS-specific ABI changes
// cannot silently fall back to an architecture-only implementation.
type TypeInfoWindowsArm64 struct {
	*TypeInfoArm64
}

// TypeInfoWindows386 implements the Microsoft x86 structure-return rules and
// the cdecl aggregate parameter rules emitted by Clang for the MSVC target.
type TypeInfoWindows386 struct {
	*Transformer
}

func (p *TypeInfoWindows386) SupportByVal() bool {
	return true
}

func (p *TypeInfoWindows386) SkipEmptyParams() bool {
	return false
}

func (p *TypeInfoWindows386) IsWrapType(ctx llvm.Context, ftyp llvm.Type, typ llvm.Type, index int) bool {
	return isAggregateType(typ)
}

func (p *TypeInfoWindows386) GetTypeInfo(ctx llvm.Context, ftyp llvm.Type, typ llvm.Type, index int) *TypeInfo {
	bret := index == 0
	info := &TypeInfo{Type: typ, Type1: typ}
	if typ.TypeKind() == llvm.VoidTypeKind {
		info.Kind = AttrVoid
		return info
	}
	if !isAggregateType(typ) {
		return info
	}

	info.Size = p.Sizeof(typ)
	info.Align = p.Alignof(typ)
	if info.Size == 0 {
		if bret {
			info.Kind = AttrVoid
			info.Type1 = ctx.VoidType()
		} else {
			info.Kind = AttrPointer
			info.Type1 = llvm.PointerType(typ, 0)
			info.ByValAlign = 4
		}
		return info
	}
	if bret {
		if isRegisterAggregateSize(info.Size) {
			info.Kind = AttrWidthType
			info.Type1 = windows386ReturnType(ctx, typ, info.Size)
		} else {
			info.Kind = AttrPointer
			info.Type1 = llvm.PointerType(typ, 0)
		}
		return info
	}

	// MSVC x86 passes a small structure's direct register-sized scalar
	// members as separate arguments. Arrays and structures that contain
	// smaller integers or nested aggregates remain indirect byval arguments.
	if typ.TypeKind() == llvm.StructTypeKind && info.Size <= 16 {
		subs := typ.StructElementTypes()
		if windows386CanExtract(subs) && p.unpaddedSize(subs) == info.Size {
			if len(subs) == 1 {
				info.Kind = AttrWidthType
				info.Type1 = subs[0]
			} else {
				info.Kind = AttrExtract
			}
			return info
		}
	}
	info.Kind = AttrPointer
	info.Type1 = llvm.PointerType(typ, 0)
	info.ByValAlign = 4
	return info
}

func (p *TypeInfoWindows386) unpaddedSize(types []llvm.Type) int {
	size := 0
	for _, typ := range types {
		size += p.Sizeof(typ)
	}
	return size
}

func isAggregateType(typ llvm.Type) bool {
	switch typ.TypeKind() {
	case llvm.ArrayTypeKind, llvm.StructTypeKind:
		return true
	}
	return false
}

func isRegisterAggregateSize(size int) bool {
	return size == 1 || size == 2 || size == 4 || size == 8
}

func windows386ReturnType(ctx llvm.Context, typ llvm.Type, size int) llvm.Type {
	if typ.TypeKind() == llvm.StructTypeKind {
		subs := typ.StructElementTypes()
		if len(subs) == 1 && subs[0].TypeKind() == llvm.PointerTypeKind && size == 4 {
			return subs[0]
		}
	}
	return ctx.IntType(size * 8)
}

func windows386CanExtract(types []llvm.Type) bool {
	if len(types) == 0 {
		return false
	}
	for _, typ := range types {
		switch typ.TypeKind() {
		case llvm.FloatTypeKind, llvm.DoubleTypeKind, llvm.PointerTypeKind:
		case llvm.IntegerTypeKind:
			if width := typ.IntTypeWidth(); width != 32 && width != 64 {
				return false
			}
		default:
			return false
		}
	}
	return true
}
