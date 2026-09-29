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
	"github.com/xgo-dev/llgo/ssa/ssatest"
	"golang.org/x/tools/go/ssa"
)

func init() {
	llssa.Initialize(llssa.InitAll | llssa.InitNative)
}

func TestStripLargeStaticByteArraySkipsSSAStores(t *testing.T) {
	const n = minStripStaticByteArray
	src := fmt.Sprintf("package p\n\nvar Table = [%d]byte{0: 1, %d: 2}\n\nfunc Use() byte { return Table[0] + Table[%d] }\n", n, n-1, n-1)
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
	payloads := StripLargeStaticByteArrays("p", []*ast.File{file})
	got := payloads[StaticByteArrayKey("p", "Table")]
	if len(got) != n || got[0] != 1 || got[n-1] != 2 {
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
	if got := StripLargeStaticByteArrays("p", []*ast.File{file}); got != nil {
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
	payloads := StripLargeStaticByteArrays("p", []*ast.File{file})
	if len(payloads[StaticByteArrayKey("p", "Table")]) != minStripStaticByteArray {
		t.Fatalf("ellipsis payload len = %d, want %d", len(payloads["Table"]), minStripStaticByteArray)
	}
	for i, b := range payloads[StaticByteArrayKey("p", "Table")] {
		if b != 1 {
			t.Fatalf("payload[%d] = %d, want 1", i, b)
		}
	}
}

func TestStripLargeStaticByteArrayIdempotent(t *testing.T) {
	const n = minStripStaticByteArray
	src := fmt.Sprintf("package p\n\nvar Fixed = [%d]byte{0: 9, %d: 8}\nvar Ellipsis = [...]byte{%s}\n", n, n-1, strings.Repeat("7, ", n))
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, "p.go", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	files := []*ast.File{file}
	first := StripLargeStaticByteArrays("p", files)
	fixed := first[StaticByteArrayKey("p", "Fixed")]
	ellipsis := first[StaticByteArrayKey("p", "Ellipsis")]
	if len(fixed) != n || fixed[0] != 9 || fixed[n-1] != 8 {
		t.Fatalf("first Fixed payload = len %d", len(fixed))
	}
	if len(ellipsis) != n || ellipsis[0] != 7 {
		t.Fatalf("first Ellipsis payload = len %d", len(ellipsis))
	}
	second := StripLargeStaticByteArrays("p", files)
	if second != nil {
		t.Fatalf("second strip should skip empty literals, got %d keys", len(second))
	}
	merged := MergeStaticByteArrays(first, second)
	if merged[StaticByteArrayKey("p", "Fixed")][0] != 9 || merged[StaticByteArrayKey("p", "Ellipsis")][0] != 7 {
		t.Fatal("merging a nil second strip overwrote payloads")
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
