//go:build !llgo

package cabi

import (
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/internal/debuginfo"
	"github.com/xgo-dev/llvm"
)

func TestParamHomeReusePreservesDebugStorage(t *testing.T) {
	ctx := llvm.NewContext()
	defer ctx.Dispose()
	mod := ctx.NewModule("cabi-debug")
	defer mod.Dispose()

	di := debuginfo.New(mod, debuginfo.Config{Producer: "LLGo"})
	cu := di.CompileUnit("cabi.go", "/src")
	file := di.File("/src/cabi.go")
	intType := di.CreateBasicType(llvm.DIBasicType{Name: "int", SizeInBits: 64, Encoding: 5})
	subroutine := di.CreateSubroutineType(llvm.DISubroutineType{File: file})
	subprogram := di.CreateFunction(cu, llvm.DIFunction{
		Name:         "cabi",
		LinkageName:  "cabi",
		File:         file,
		Line:         1,
		ScopeLine:    1,
		Type:         subroutine,
		IsDefinition: true,
	})
	variable := di.CreateAutoVariable(subprogram, llvm.DIAutoVariable{
		Name:           "value",
		File:           file,
		Line:           1,
		Type:           intType,
		AlwaysPreserve: true,
	})

	int64Type := ctx.Int64Type()
	fnType := llvm.FunctionType(ctx.VoidType(), []llvm.Type{int64Type, llvm.PointerType(int64Type, 0)}, false)
	fn := llvm.AddFunction(mod, "cabi", fnType)
	fn.SetSubprogram(subprogram)
	param := fn.Param(0)
	param.SetName("param")
	replacement := fn.Param(1)
	replacement.SetName("replacement")
	builder := ctx.NewBuilder()
	defer builder.Dispose()
	block := llvm.AddBasicBlock(fn, "entry")
	builder.SetInsertPointAtEnd(block)
	home := builder.CreateAlloca(int64Type, "home")
	di.InsertDeclareAtEnd(home, variable, di.CreateExpression(nil), llvm.DebugLoc{Line: 1, Scope: subprogram}, block)
	builder.CreateStore(param, home)
	loaded := builder.CreateLoad(int64Type, home, "loaded")
	builder.CreateStore(loaded, replacement)
	builder.CreateRetVoid()

	reuseParamHome(param, replacement, block, 8, 8)
	di.Finalize()
	if err := llvm.VerifyModule(mod, llvm.ReturnStatusAction); err != nil {
		t.Fatalf("rewritten module is invalid: %v\n%s", err, mod.String())
	}
	ir := mod.String()
	if !strings.Contains(ir, "#dbg_declare(ptr %home") {
		t.Fatalf("dbg.declare lost its authoritative home:\n%s", ir)
	}
	if !strings.Contains(ir, "%loaded = load i64, ptr %home") {
		t.Fatalf("executable uses no longer read the declared home:\n%s", ir)
	}
}
