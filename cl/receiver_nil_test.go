//go:build !llgo

package cl

import (
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"strings"
	"testing"

	"github.com/goplus/gogen/packages"
	"github.com/xgo-dev/llvm"
	gossa "golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"
)

func TestPointerMethodReceiverNilChecks(t *testing.T) {
	const source = `package foo
type T struct { x int }
func (*T) M(int) {}
type U struct { pad int; T }
type P struct { *T }
func arg() int { return 1 }
func plain(p *T) { p.M(arg()) }
func expression(p *T) { (*T).M(p, arg()) }
func deref(p *T) { (*p).M(arg()) }
func promoted(p *U) { p.M(arg()) }
func pointer(p *P) { p.M(arg()) }
func deferred(p *T) { defer (*p).M(arg()) }
func started(p *T) { go (*p).M(arg()) }
func bound(p *T) func(int) { return (*p).M }
func shadowed(T *T) { (*T).M(arg()) }
func promotedExpression(p *U) { (*U).M(p, arg()) }
func savedPromotedExpression(p *P) { f := (*P).M; f(p, arg()) }
`
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "receiver.go", source, 0)
	if err != nil {
		t.Fatal(err)
	}
	files := []*ast.File{file}
	ssaPkg, info, err := ssautil.BuildPackage(&types.Config{Importer: packages.NewImporter(fset)}, fset,
		types.NewPackage("foo", "foo"), files, gossa.SanityCheckFunctions)
	if err != nil {
		t.Fatal(err)
	}
	checks := CollectReceiverNilChecks(files, nil, info)
	if checks == nil || len(checks.calls) != 7 || len(checks.values) != 8 {
		t.Fatalf("receiver check collection = %#v, want 7 calls and 8 selections", checks)
	}
	if CollectReceiverNilChecks(files, nil) != nil || receiverNeedsAddressCheck(nil) {
		t.Fatal("absent type information must not invent receiver checks")
	}
	prog := newLLSSAProg(t)
	defer prog.Dispose()
	pkg, _, err := newPackageEx(prog, nil, nil, nil, ssaPkg, files, nil, false, Options{ReceiverNilChecks: checks})
	if err != nil {
		t.Fatal(err)
	}
	mod := pkg.Module()
	if err := llvm.VerifyModule(mod, llvm.ReturnStatusAction); err != nil {
		t.Fatal(err)
	}
	for _, name := range []string{"plain", "expression"} {
		ir := mustNamedFunction(t, mod, "foo."+name).String()
		if strings.Contains(ir, "AssertNilDeref") {
			t.Fatalf("legal pointer receiver %s gained a nil check:\n%s", name, ir)
		}
	}
	for _, name := range []string{"deref", "promoted", "deferred", "started", "shadowed"} {
		ir := mustNamedFunction(t, mod, "foo."+name).String()
		argument := strings.Index(ir, "@foo.arg(")
		guard := strings.Index(ir, "AssertNilDeref")
		if argument < 0 || guard < argument {
			t.Fatalf("%s must check its receiver after evaluating arguments:\n%s", name, ir)
		}
	}
	// An embedded-pointer receiver has a real load, unlike address-only
	// promotion. Its guard must dominate that load even when optimization
	// removes the eventual method's unused receiver argument.
	pointerIR := mustNamedFunction(t, mod, "foo.pointer").String()
	guard := strings.Index(pointerIR, "AssertNilDerefPtr")
	load := strings.Index(pointerIR, "load ptr")
	if guard < 0 || load < guard {
		t.Fatalf("embedded receiver pointer load precedes its guard:\n%s", pointerIR)
	}
	ir := mustNamedFunction(t, mod, "foo.bound").String()
	if !strings.Contains(ir, "AssertNilDeref") {
		t.Fatalf("method-value creation lost its nil check:\n%s", ir)
	}
}

func TestImportedGenericPointerMethodReceiverNilChecks(t *testing.T) {
	fset := token.NewFileSet()
	check := func(path, source string, importer types.Importer) (*types.Package, *types.Info, *ast.File) {
		t.Helper()
		file, err := parser.ParseFile(fset, path+".go", source, 0)
		if err != nil {
			t.Fatal(err)
		}
		info := newLocalityTypeInfo()
		pkg, err := (&types.Config{Importer: importer}).Check(path, fset, []*ast.File{file}, info)
		if err != nil {
			t.Fatal(err)
		}
		return pkg, info, file
	}
	dep, depInfo, depFile := check("example.com/dep", `package dep
type T struct{}
func (*T) M(int) {}
func Call[A any](p *T, arg func() int, _ A) { (*p).M(arg()) }
func Bound[A any](p *T, _ A) func(int) { return (*p).M }
`, nil)
	root, rootInfo, rootFile := check("example.com/root", `package root
import "example.com/dep"
func Invoke(p *dep.T, arg func() int) { dep.Call(p, arg, 1); dep.Bound(p, 1)(arg()) }
`, importerFunc(func(path string) (*types.Package, error) {
		if path == dep.Path() {
			return dep, nil
		}
		return nil, types.Error{Msg: "unexpected import " + path}
	}))
	goProg := gossa.NewProgram(fset, gossa.SanityCheckFunctions|gossa.InstantiateGenerics)
	goProg.CreatePackage(dep, []*ast.File{depFile}, depInfo, true)
	rootSSA := goProg.CreatePackage(root, []*ast.File{rootFile}, rootInfo, true)
	goProg.Build()
	if checks := CollectReceiverNilChecks([]*ast.File{rootFile}, rootInfo); checks != nil {
		t.Fatal("the caller's syntax must not contain the dependency's receiver selections")
	}
	checks := CollectReceiverNilChecks([]*ast.File{rootFile, depFile}, rootInfo, depInfo)
	prog := newLLSSAProg(t)
	defer prog.Dispose()
	pkg, _, err := newPackageEx(prog, nil, nil, nil, rootSSA, []*ast.File{rootFile}, nil, false, Options{ReceiverNilChecks: checks})
	if err != nil {
		t.Fatal(err)
	}
	mod := pkg.Module()
	if err := llvm.VerifyModule(mod, llvm.ReturnStatusAction); err != nil {
		t.Fatal(err)
	}
	for _, name := range []string{"Call", "Bound"} {
		found := false
		for fn := mod.FirstFunction(); !fn.IsNil(); fn = llvm.NextFunction(fn) {
			if !strings.HasPrefix(fn.Name(), "example.com/dep."+name+"[") {
				continue
			}
			found = true
			if !strings.Contains(fn.String(), "AssertNilDeref") {
				t.Fatalf("imported generic %s lost its source receiver check:\n%s", name, fn.String())
			}
		}
		if !found {
			t.Fatalf("imported generic %s was not emitted in the caller module:\n%s", name, mod.String())
		}
	}
}
