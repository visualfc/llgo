//go:build windows

package ffi

import (
	"runtime"
	"testing"
	"unsafe"

	cliteffi "github.com/xgo-dev/llgo/runtime/internal/clite/ffi"
)

func TestWindowsNativeABIs(t *testing.T) {
	wantDefault := ABI(0)
	wantStdcall := ABI(0)
	switch runtime.GOARCH {
	case "386":
		wantDefault = 5 // FFI_MS_CDECL
		wantStdcall = 2 // FFI_STDCALL
	case "amd64":
		wantDefault = 1 // FFI_WIN64
		wantStdcall = wantDefault
	case "arm64":
		wantDefault = 2 // FFI_WIN64
		wantStdcall = wantDefault
	default:
		t.Fatalf("unexpected Windows architecture %q", runtime.GOARCH)
	}
	if DefaultABI != wantDefault {
		t.Fatalf("DefaultABI = %d, want %d", DefaultABI, wantDefault)
	}
	if WindowsStdcallABI != wantStdcall {
		t.Fatalf("WindowsStdcallABI = %d, want %d", WindowsStdcallABI, wantStdcall)
	}
}

func TestWindowsComplexTypesUseAggregateABI(t *testing.T) {
	tests := []struct {
		name string
		typ  *Type
		elem *Type
	}{
		{"complex64", TypeComplex64, TypeFloat32},
		{"complex128", TypeComplex128, TypeFloat64},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			if test.typ.Type != cliteffi.Struct {
				t.Fatalf("libffi type = %d, want struct", test.typ.Type)
			}
			if test.typ.Elements == nil {
				t.Fatal("aggregate has no element list")
			}
			elements := unsafe.Slice(test.typ.Elements, 3)
			if elements[0] != test.elem || elements[1] != test.elem || elements[2] != nil {
				t.Fatalf("aggregate elements = %p, %p, %p; want %p, %p, nil",
					elements[0], elements[1], elements[2], test.elem, test.elem)
			}
		})
	}
}
