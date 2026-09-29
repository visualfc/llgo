package cl

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"go/types"
	"strings"
	"testing"

	gpackages "github.com/goplus/gogen/packages"
	llssa "github.com/xgo-dev/llgo/ssa"
	"github.com/xgo-dev/llgo/ssa/abi"
	"github.com/xgo-dev/llgo/ssa/ssatest"
	"golang.org/x/tools/go/ssa"
)

func init() {
	llssa.Initialize(llssa.InitAll | llssa.InitNative)
}

func TestStripLargeStaticByteArraySkipsSSAStores(t *testing.T) {
	const n = minStripStaticByteArray
	src := fmt.Sprintf("package p\n\nvar Table = [%d]byte{%s}\n\nfunc Use() byte { return Table[0] + Table[%d] }\n", n, strings.Repeat("1, ", n), n-1)
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "p.go", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	info := &types.Info{
		Types: make(map[ast.Expr]types.TypeAndValue),
		Defs:  make(map[*ast.Ident]types.Object),
		Uses:  make(map[*ast.Ident]types.Object),
	}
	conf := types.Config{Importer: gpackages.NewImporter(fset)}
	pkg, err := conf.Check("p", fset, []*ast.File{file}, info)
	if err != nil {
		t.Fatal(err)
	}
	payloads := StripLargeStaticByteArrays(pkg, []*ast.File{file})
	got := payloads[StaticByteArrayKey(llssa.PathOf(pkg), "Table")]
	if len(got) != n || got[0] != 1 || got[n-1] != 1 {
		t.Fatalf("payload = len %d data[0]=%d data[%d]=%d", len(got), got[0], n-1, got[n-1])
	}
	cl, ok := file.Decls[0].(*ast.GenDecl).Specs[0].(*ast.ValueSpec).Values[0].(*ast.CompositeLit)
	if !ok || len(cl.Elts) != 0 {
		t.Fatalf("composite literal was not stripped: %#v", cl)
	}

	prog := ssa.NewProgram(fset, ssa.SanityCheckFunctions|ssa.InstantiateGenerics)
	ssaPkg := prog.CreatePackage(pkg, []*ast.File{file}, info, true)
	ssaPkg.Build()
	initFn := ssaPkg.Func("init")
	stores := 0
	if initFn != nil {
		for _, b := range initFn.Blocks {
			for _, instr := range b.Instrs {
				if _, ok := instr.(*ssa.Store); ok {
					stores++
				}
			}
		}
	}
	if stores > 8 {
		t.Fatalf("init still has %d stores after stripping %d-byte array", stores, n)
	}

	llprog := ssatest.NewProgramEx(t, nil, conf.Importer)
	llprog.TypeSizes(types.SizesFor("gc", "arm64"))
	ret, _, err := NewPackageExWithEmbedMetaOptions(llprog, nil, nil, nil, ssaPkg, []*ast.File{file}, nil, false, Options{StaticByteArrays: payloads})
	if err != nil {
		t.Fatal(err)
	}
	ir := ret.String()
	if !strings.Contains(ir, fmt.Sprintf("@p.Table = global [%d x i8]", n)) {
		t.Fatalf("missing static byte array global:\n%s", ir)
	}
	if strings.Contains(ir, "zeroinitializer") {
		t.Fatalf("static byte array still zero-initialized:\n%s", ir)
	}
}

func TestStripLargeStaticByteArrayIgnoresSmallTables(t *testing.T) {
	src := "package p\n\nvar SBox = [256]byte{1, 2, 3}\n"
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "p.go", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	if got := StripLargeStaticByteArrays(types.NewPackage("p", "p"), []*ast.File{file}); got != nil {
		t.Fatalf("small table was stripped: %v", got)
	}
	cl := file.Decls[0].(*ast.GenDecl).Specs[0].(*ast.ValueSpec).Values[0].(*ast.CompositeLit)
	if len(cl.Elts) != 3 {
		t.Fatalf("small table elts = %d, want 3", len(cl.Elts))
	}
}

func TestStripLargeStaticByteArrayEllipsis(t *testing.T) {
	elts := strings.Repeat("0x01, ", minStripStaticByteArray)
	src := "package p\n\nvar Table = [...]byte{" + elts + "}\n"
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "p.go", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	pkg := types.NewPackage("p", "p")
	payloads := StripLargeStaticByteArrays(pkg, []*ast.File{file})
	if len(payloads[StaticByteArrayKey(llssa.PathOf(pkg), "Table")]) != minStripStaticByteArray {
		t.Fatalf("ellipsis payload len = %d, want %d", len(payloads["Table"]), minStripStaticByteArray)
	}
	for i, b := range payloads[StaticByteArrayKey(llssa.PathOf(pkg), "Table")] {
		if b != 1 {
			t.Fatalf("payload[%d] = %d, want 1", i, b)
		}
	}
}

func TestStripLargeStaticByteArrayIdempotent(t *testing.T) {
	const n = minStripStaticByteArray
	src := fmt.Sprintf("package p\n\nvar Fixed = [%d]byte{%s}\nvar Ellipsis = [...]byte{%s}\n", n, strings.Repeat("9, ", n), strings.Repeat("7, ", n))
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "p.go", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	files := []*ast.File{file}
	pkg := types.NewPackage("p", "p")
	first := StripLargeStaticByteArrays(pkg, files)
	fixed := first[StaticByteArrayKey(llssa.PathOf(pkg), "Fixed")]
	ellipsis := first[StaticByteArrayKey(llssa.PathOf(pkg), "Ellipsis")]
	if len(fixed) != n || fixed[0] != 9 || fixed[n-1] != 9 {
		t.Fatalf("first Fixed payload = len %d", len(fixed))
	}
	if len(ellipsis) != n || ellipsis[0] != 7 {
		t.Fatalf("first Ellipsis payload = len %d", len(ellipsis))
	}
	second := StripLargeStaticByteArrays(pkg, files)
	if second != nil {
		t.Fatalf("second strip should skip empty literals, got %d keys", len(second))
	}
	merged := MergeStaticByteArrays(first, second)
	if merged[StaticByteArrayKey(llssa.PathOf(pkg), "Fixed")][0] != 9 || merged[StaticByteArrayKey(llssa.PathOf(pkg), "Ellipsis")][0] != 7 {
		t.Fatal("merging a nil second strip overwrote payloads")
	}
}

func TestStripLargeStaticByteArraySkipsSparse(t *testing.T) {
	const n = minStripStaticByteArray
	src := fmt.Sprintf("package p\n\nvar Table = [%d]byte{0: 1, %d: 2}\n", n, n-1)
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "p.go", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	if got := StripLargeStaticByteArrays(types.NewPackage("p", "p"), []*ast.File{file}); got != nil {
		t.Fatalf("sparse table was stripped: %v", got)
	}
	cl := file.Decls[0].(*ast.GenDecl).Specs[0].(*ast.ValueSpec).Values[0].(*ast.CompositeLit)
	if len(cl.Elts) != 2 {
		t.Fatalf("sparse table elts = %d, want 2", len(cl.Elts))
	}
}

func TestStripLargeStaticByteArrayUsesPathOf(t *testing.T) {
	const n = minStripStaticByteArray
	src := fmt.Sprintf("package main\n\nvar Table = [%d]byte{%s}\n", n, strings.Repeat("1, ", n))
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "main.go", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	pkg := types.NewPackage("command-line-arguments", "main")
	payloads := StripLargeStaticByteArrays(pkg, []*ast.File{file})
	if payloads[StaticByteArrayKey("command-line-arguments", "Table")] != nil {
		t.Fatal("strip key used types.Path instead of PathOf")
	}
	got := payloads[StaticByteArrayKey(llssa.PathOf(pkg), "Table")]
	if len(got) != n || llssa.PathOf(pkg) != "main" {
		t.Fatalf("main payload key = %q len %d", llssa.PathOf(pkg), len(got))
	}

	patched := types.NewPackage(abi.PatchPathPrefix+"runtime", "runtime")
	src2 := fmt.Sprintf("package runtime\n\nvar Table = [%d]byte{%s}\n", n, strings.Repeat("2, ", n))
	file2, err := parser.ParseFile(fset, "runtime.go", src2, 0)
	if err != nil {
		t.Fatal(err)
	}
	payloads = StripLargeStaticByteArrays(patched, []*ast.File{file2})
	if payloads[StaticByteArrayKey(abi.PatchPathPrefix+"runtime", "Table")] != nil {
		t.Fatal("strip key used untrimmed patch path")
	}
	got = payloads[StaticByteArrayKey(llssa.PathOf(patched), "Table")]
	if len(got) != n || got[0] != 2 || llssa.PathOf(patched) != "runtime" {
		t.Fatalf("patched payload PathOf=%q len %d data[0]=%d", llssa.PathOf(patched), len(got), got[0])
	}
}

func TestMergeStaticByteArraysQualifiedKeys(t *testing.T) {
	dst := map[string][]byte{StaticByteArrayKey("a", "T"): {1}}
	src := map[string][]byte{StaticByteArrayKey("b", "T"): {2}}
	got := MergeStaticByteArrays(dst, src)
	if got[StaticByteArrayKey("a", "T")][0] != 1 || got[StaticByteArrayKey("b", "T")][0] != 2 {
		t.Fatalf("qualified keys collided: %v", got)
	}
}
