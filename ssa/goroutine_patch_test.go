//go:build !llgo
// +build !llgo

package ssa_test

import (
	"go/types"
	"strings"
	"testing"

	"github.com/goplus/llgo/ssa"
	"github.com/goplus/llgo/ssa/ssatest"
	"github.com/xgo-dev/llvm"
)

func TestGoClosureStartupUsesGCManagedMemory(t *testing.T) {
	prog := ssatest.NewProgram(t, nil)
	pkg := prog.NewPackage("bar", "foo/bar")

	ctxFields := []*types.Var{
		types.NewField(0, nil, "x", types.Typ[types.Int], false),
	}
	ctxStruct := types.NewStruct(ctxFields, nil)
	ctxParam := types.NewParam(0, nil, "__llgo_ctx", types.NewPointer(ctxStruct))
	innerSig := types.NewSignatureType(nil, nil, nil, types.NewTuple(ctxParam), nil, false)
	inner := pkg.NewFunc("inner", innerSig, ssa.InGo)
	ib := inner.MakeBody(1)
	ib.Return()

	outer := pkg.NewFunc("outer", ssa.NoArgsNoRet, ssa.InGo)
	ob := outer.MakeBody(1)
	closure := ob.MakeClosure(inner.Expr, []ssa.Expr{prog.Val(42)})
	ob.Go(closure, func(b ssa.Builder, fn ssa.Expr, args ...ssa.Expr) ssa.Expr {
		return b.Call(fn, args...)
	})
	ob.Return()

	ir := pkg.String()
	if strings.Contains(ir, "@malloc") {
		t.Fatalf("goroutine startup data should not use malloc:\n%s", ir)
	}
	if strings.Contains(ir, "@free") {
		t.Fatalf("goroutine startup data should not use free:\n%s", ir)
	}
	if !strings.Contains(ir, `"github.com/goplus/llgo/runtime/internal/runtime.AllocRoot"`) {
		t.Fatalf("goroutine startup data should use scanned uncollectable memory:\n%s", ir)
	}
	if !strings.Contains(ir, `"github.com/goplus/llgo/runtime/internal/runtime.FreeRoot"`) {
		t.Fatalf("goroutine startup data should be freed after the entry call returns:\n%s", ir)
	}
	// The closure context must remain visible to the runtime GC until the
	// uncollectable startup record is initialized.
	if got := strings.Count(ir, `"github.com/goplus/llgo/runtime/internal/runtime.AllocU"`); got < 1 {
		t.Fatalf("expected closure ctx to use AllocU, got %d:\n%s", got, ir)
	}
}

func TestGoPanicRoutineDoesNotReturnAfterUnreachable(t *testing.T) {
	prog := ssatest.NewProgram(t, nil)
	pkg := prog.NewPackage("bar", "foo/bar")

	outer := pkg.NewFunc("outer", ssa.NoArgsNoRet, ssa.InGo)
	ob := outer.MakeBody(1)
	ob.Go(ssa.Nil, func(b ssa.Builder, _ ssa.Expr, args ...ssa.Expr) ssa.Expr {
		b.Panic(args[0])
		return ssa.Expr{}
	}, prog.Zero(prog.Any()))
	ob.Return()

	if err := llvm.VerifyModule(pkg.Module(), llvm.ReturnStatusAction); err != nil {
		t.Fatal(err)
	}

	ir := pkg.String()
	freeRoot := strings.Index(ir, `"github.com/goplus/llgo/runtime/internal/runtime.FreeRoot"`)
	panicCall := strings.Index(ir, `"github.com/goplus/llgo/runtime/internal/runtime.Panic"`)
	if freeRoot < 0 || panicCall < 0 || freeRoot > panicCall {
		t.Fatalf("goroutine wrapper should free startup data before panic call:\n%s", ir)
	}
	if strings.Contains(ir, "unreachable\n  ret ptr null") {
		t.Fatalf("goroutine wrapper should not return after unreachable:\n%s", ir)
	}
}

func TestGoUsesPthreadAttrForDetachedStack(t *testing.T) {
	prog := ssatest.NewProgram(t, nil)
	prog.SetPthreadStackSize(32 << 20)
	pkg := prog.NewPackage("bar", "foo/bar")

	outer := pkg.NewFunc("outer", ssa.NoArgsNoRet, ssa.InGo)
	ob := outer.MakeBody(1)
	ob.Go(ssa.Nil, func(b ssa.Builder, _ ssa.Expr, args ...ssa.Expr) ssa.Expr {
		return ssa.Expr{}
	})
	ob.Return()

	ir := pkg.String()
	initAttr := strings.Index(ir, `"github.com/goplus/llgo/runtime/internal/runtime.InitThreadAttrWithStack"`)
	createThread := strings.Index(ir, `"github.com/goplus/llgo/runtime/internal/runtime.CreateThread"`)
	destroyAttr := strings.Index(ir, `"github.com/goplus/llgo/runtime/internal/runtime.DestroyThreadAttr"`)
	if initAttr < 0 || createThread < 0 || destroyAttr < 0 {
		t.Fatalf("goroutine should initialize, use, and destroy pthread attrs:\n%s", ir)
	}
	if !(initAttr < createThread && createThread < destroyAttr) {
		t.Fatalf("pthread attr calls should wrap CreateThread:\n%s", ir)
	}
	if !strings.Contains(ir, "33554432") {
		t.Fatalf("goroutine should pass configured pthread stack size:\n%s", ir)
	}
	if strings.Contains(ir, "llgo_pthread_create_detached") {
		t.Fatalf("goroutine should not call detached C wrapper:\n%s", ir)
	}
}

func TestGoUsesPthreadAttrWithoutStackByDefault(t *testing.T) {
	prog := ssatest.NewProgram(t, nil)
	pkg := prog.NewPackage("bar", "foo/bar")

	outer := pkg.NewFunc("outer", ssa.NoArgsNoRet, ssa.InGo)
	ob := outer.MakeBody(1)
	ob.Go(ssa.Nil, func(b ssa.Builder, _ ssa.Expr, args ...ssa.Expr) ssa.Expr {
		return ssa.Expr{}
	})
	ob.Return()

	ir := pkg.String()
	if !strings.Contains(ir, `"github.com/goplus/llgo/runtime/internal/runtime.InitThreadAttr"`) {
		t.Fatalf("goroutine should initialize pthread attrs:\n%s", ir)
	}
	if strings.Contains(ir, `"github.com/goplus/llgo/runtime/internal/runtime.InitThreadAttrWithStack"`) {
		t.Fatalf("default goroutine should not request custom pthread stack size:\n%s", ir)
	}
}
