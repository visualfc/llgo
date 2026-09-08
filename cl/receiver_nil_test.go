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
