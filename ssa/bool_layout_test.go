//go:build !llgo

package ssa

import (
	"go/token"
	"go/types"
	"strings"
	"testing"
	"unsafe"
)

func TestBoolLLVMLayoutMatchesGoAndC(t *testing.T) {
	prog := NewProgram(nil)
	defer prog.Dispose()
	boolTy := prog.Bool()
	if got, want := boolTy.ll.IntTypeWidth(), 8; got != want {
		t.Fatalf("Go bool LLVM width = %d, want %d (i8)", got, want)
	}
	if got, want := prog.td.TypeAllocSize(boolTy.ll), uint64(1); got != want {
		t.Fatalf("Go bool alloc size = %d, want %d", got, want)
	}
	if unsafe.Sizeof(true) != 1 {
		t.Fatalf("unsafe.Sizeof(true) = %d, want 1", unsafe.Sizeof(true))
	}

	arr := prog.rawType(types.NewArray(types.Typ[types.Bool], 256))
	if got, want := prog.td.TypeAllocSize(arr.ll), uint64(256); got != want {
		t.Fatalf("[256]bool alloc size = %d, want %d (Go and C _Bool[256] are 256 bytes, not a packed i1 bitvector)", got, want)
	}
	if unsafe.Sizeof([256]bool{}) != 256 {
		t.Fatalf("unsafe.Sizeof([256]bool) = %d, want 256", unsafe.Sizeof([256]bool{}))
	}

	fields := []*types.Var{
		types.NewVar(token.NoPos, nil, "a", types.Typ[types.Bool]),
		types.NewVar(token.NoPos, nil, "b", types.Typ[types.Uint8]),
		types.NewVar(token.NoPos, nil, "c", types.Typ[types.Bool]),
	}
	st := prog.rawType(types.NewStruct(fields, nil))
	type goLayout struct {
		a bool
		b uint8
		c bool
	}
	if got, want := prog.td.TypeAllocSize(st.ll), uint64(unsafe.Sizeof(goLayout{})); got != want {
		t.Fatalf("struct{bool; uint8; bool} alloc size = %d, want %d", got, want)
	}

	// C _Bool is 1 byte; _Bool[256] is 256 bytes; struct {_Bool; char; _Bool} is 3.
	// Those match Go bool / [256]bool / struct{bool; byte; bool}. Clang still
	// uses zeroext i1 for scalar _Bool in IR, but the machine ABI is an 8-bit
	// 0/1 in a register — the same as i8.
	if got, want := uint64(unsafe.Sizeof(goLayout{})), uint64(3); got != want {
		t.Fatalf("Go struct size = %d, want 3 to match C struct {_Bool; char; _Bool}", got)
	}

	pkg := prog.NewPackage("abi", "abi")
	sig := types.NewSignatureType(nil, nil, nil, types.NewTuple(types.NewVar(token.NoPos, nil, "b", types.Typ[types.Bool])), types.NewTuple(types.NewVar(token.NoPos, nil, "", types.Typ[types.Bool])), false)
	fn := pkg.NewFunc("Neg", sig, InGo)
	b := fn.MakeBody(1)
	b.Return(b.UnOp(token.NOT, fn.Param(0)))
	ir := pkg.String()
	if !strings.Contains(ir, "define i8 @Neg(i8 %0)") {
		t.Fatalf("Go bool function is not i8:\n%s", ir)
	}
	if strings.Contains(ir, "define i1 @Neg") {
		t.Fatalf("Go bool function still uses i1:\n%s", ir)
	}
}
