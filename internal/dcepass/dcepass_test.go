package dcepass

import (
	"os"
	"strings"
	"testing"

	"github.com/xgo-dev/llvm"
)

func TestEmitStrongTypeOverridesPrunesDeadMethodSlots(t *testing.T) {
	ctx := llvm.NewContext()
	defer ctx.Dispose()
	src := ctx.NewModule("src")
	defer src.Dispose()
	dst := ctx.NewModule("dst")
	defer dst.Dispose()

	addMethodTypeGlobal(src, "_llgo_pkg.T", "M", "N")
	if err := EmitStrongTypeOverrides(dst, []llvm.Module{src}, map[string][]int{"_llgo_pkg.T": {0}}, false); err != nil {
		t.Fatal(err)
	}

	out := dst.String()
	if !strings.Contains(out, `@_llgo_pkg.T = constant`) {
		t.Fatalf("override type global was not emitted:\n%s", out)
	}
	if !strings.Contains(out, `ptr @"pkg.(*T).M", ptr @pkg.T.M`) {
		t.Fatalf("live method slot was not preserved:\n%s", out)
	}
	if strings.Contains(out, `ptr @"pkg.(*T).N"`) || strings.Contains(out, `ptr @pkg.T.N`) {
		t.Fatalf("dead method slot still references N functions:\n%s", out)
	}
	if !strings.Contains(out, `ptr @"`+unreachableMethodName+`"`) {
		t.Fatalf("dead method slot was not redirected to unreachableMethod:\n%s", out)
	}
}

func TestEmitStrongTypeOverridesLogsDroppedMethodSlots(t *testing.T) {
	ctx := llvm.NewContext()
	defer ctx.Dispose()
	src := ctx.NewModule("src")
	defer src.Dispose()
	dst := ctx.NewModule("dst")
	defer dst.Dispose()

	addMethodTypeGlobal(src, "_llgo_pkg.T", "M")
	logFile, err := os.CreateTemp(t.TempDir(), "dcepass-stderr")
	if err != nil {
		t.Fatal(err)
	}
	oldStderr := os.Stderr
	os.Stderr = logFile
	t.Cleanup(func() {
		os.Stderr = oldStderr
	})
	if err := EmitStrongTypeOverrides(dst, []llvm.Module{src}, nil, true); err != nil {
		t.Fatal(err)
	}
	if err := logFile.Close(); err != nil {
		t.Fatal(err)
	}
	log, err := os.ReadFile(logFile.Name())
	if err != nil {
		t.Fatal(err)
	}
	if want := `[dce] drop method _llgo_pkg.T[0] ifn=pkg.(*T).M tfn=pkg.T.M`; !strings.Contains(string(log), want) {
		t.Fatalf("debug log missing dropped method slot\nwant: %s\ngot:\n%s", want, log)
	}
}

func addMethodTypeGlobal(mod llvm.Module, name string, methods ...string) {
	ctx := mod.Context()
	fnTy := llvm.FunctionType(ctx.VoidType(), nil, false)
	ptrTy := llvm.PointerType(fnTy, 0)
	stringTy := ctx.StructCreateNamed("runtime/internal/runtime.String")
	stringTy.StructSetBody([]llvm.Type{llvm.PointerType(ctx.Int8Type(), 0), ctx.Int64Type()}, false)
	methodTy := ctx.StructCreateNamed("github.com/goplus/llgo/runtime/abi.Method")
	methodTy.StructSetBody([]llvm.Type{stringTy, ptrTy, ptrTy, ptrTy}, false)

	mtyp := llvm.AddGlobal(mod, ptrTy, "mtyp")
	methodValues := make([]llvm.Value, len(methods))
	for i, method := range methods {
		ifn := llvm.AddFunction(mod, "pkg.(*T)."+method, fnTy)
		tfn := llvm.AddFunction(mod, "pkg.T."+method, fnTy)
		methodValues[i] = llvm.ConstNamedStruct(methodTy, []llvm.Value{
			llvm.ConstNull(stringTy), mtyp, ifn, tfn,
		})
	}
	methodArray := llvm.ConstArray(methodTy, methodValues)
	typeTy := ctx.StructCreateNamed("pkg.T.type")
	typeTy.StructSetBody([]llvm.Type{ctx.Int8Type(), methodArray.Type()}, false)
	typeDesc := llvm.AddGlobal(mod, typeTy, name)
	typeDesc.SetGlobalConstant(true)
	typeDesc.SetLinkage(llvm.WeakODRLinkage)
	typeDesc.SetInitializer(llvm.ConstNamedStruct(typeTy, []llvm.Value{
		llvm.ConstNull(ctx.Int8Type()), methodArray,
	}))
}
