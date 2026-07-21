//go:build !llgo

package ssa

import (
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"runtime"
	"strings"
	"testing"

	"github.com/xgo-dev/llvm"
)

func TestDebugRecursiveNamedTypesFinalize(t *testing.T) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "recursive.go", `package p
type Link *Link
type Peano *Peano
func inspect() {
	if true {
		value := 1
		_ = value
	}
}
`, 0)
	if err != nil {
		t.Fatal(err)
	}
	typesPkg := types.NewPackage("example.com/p", "p")
	info := &types.Info{Scopes: make(map[ast.Node]*types.Scope)}
	if err := types.NewChecker(&types.Config{}, fset, typesPkg, info).Files([]*ast.File{file}); err != nil {
		t.Fatal(err)
	}

	prog := NewProgram(nil)
	defer prog.Dispose()
	prog.TypeSizes(types.SizesFor("gc", runtime.GOARCH))
	prog.SetDebugInfoOptimized(false)
	pkg := prog.NewPackage("p", "example.com/p")
	pkg.InitDebug("p", "example.com/p", fset)
	fn := pkg.NewFunc("debugTypes", NoArgsNoRet, InGo)
	builder := fn.NewBuilder()
	defer builder.impl.Dispose()
	for _, name := range []string{"Link", "Peano"} {
		object := typesPkg.Scope().Lookup(name)
		pos := fset.Position(object.Pos())
		global := pkg.NewVar("example.com/p."+name, types.NewPointer(object.Type()), InGo)
		builder.DIGlobal(global.Expr, name, pos)
	}

	decl := file.Decls[2].(*ast.FuncDecl)
	object := typesPkg.Scope().Lookup("inspect").(*types.Func)
	function := pkg.NewFunc("example.com/p.inspect", object.Type().(*types.Signature), InGo)
	functionBuilder := function.MakeBody(1)
	defer functionBuilder.Dispose()
	functionBuilder.DebugFunction(
		function,
		object.Scope(),
		fset.Position(object.Pos()),
		fset.Position(decl.Body.Lbrace),
	)
	if got := functionBuilder.DIScope(function, nil); got != function {
		t.Fatal("nil scope did not resolve to the function")
	}
	if got := functionBuilder.DIScope(function, object.Scope()); got != function {
		t.Fatal("function scope did not resolve to the function")
	}
	if got := functionBuilder.DIScope(function, typesPkg.Scope()); got != function {
		t.Fatal("package scope did not resolve to the function")
	}
	innerBlock := decl.Body.List[0].(*ast.IfStmt).Body
	innerScope := info.Scopes[innerBlock]
	if innerScope == nil {
		t.Fatal("inner lexical scope not found")
	}
	lexical := functionBuilder.DIScope(function, innerScope)
	if lexical == function || functionBuilder.DIScope(function, innerScope) != lexical {
		t.Fatal("inner lexical scope was not created and cached")
	}
	functionBuilder.DISetCurrentDebugLocation(lexical, fset.Position(innerBlock.Lbrace))
	functionBuilder.Return()
	functionBuilder.EndBuild()

	pkg.FinalizeDebug()
	pkg.FinalizeDebug()

	if err := llvm.VerifyModule(pkg.Module(), llvm.ReturnStatusAction); err != nil {
		t.Fatalf("recursive debug metadata is invalid: %v\n%s", err, pkg.Module().String())
	}
	ir := pkg.Module().String()
	for _, name := range []string{"Link", "Peano"} {
		if !strings.Contains(ir, `name: "`+name+`"`) {
			t.Fatalf("module is missing debug type %s:\n%s", name, ir)
		}
	}
	for _, want := range []string{"DILexicalBlock", "isOptimized: false"} {
		if !strings.Contains(ir, want) {
			t.Fatalf("module is missing %q:\n%s", want, ir)
		}
	}
}
