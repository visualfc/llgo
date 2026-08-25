//go:build !windows

package ffi

import "github.com/xgo-dev/llgo/runtime/internal/clite/ffi"

func newComplexType(elem *Type, size uintptr, align uint16) *Type {
	return &Type{size, align, ffi.Complex, &[]*Type{elem, nil}[0]}
}
