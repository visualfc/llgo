package main

import (
	"go/build"
	"go/importer"
	"go/token"
	"go/types"
)

// wrappedFunc preserves the embedded-unexported-object regression (#1598).
type wrappedFunc struct{ *types.Func }

// This case is an integration smoke for the Go tooling APIs. It intentionally
// samples each API family instead of duplicating their standard-library test
// suites in several command packages.
func main() {
	// Keep the exact address-of-global build regression (#1389).
	ctx := &build.Default
	if ctx.Compiler == "" || ctx.GOOS == "" || ctx.GOARCH == "" {
		panic("incomplete build.Default")
	}
	pkg, err := build.Import("fmt", "", build.FindOnly)
	if err != nil || pkg.ImportPath != "fmt" || !pkg.Goroot {
		panic("build.Import")
	}
	if here, err := build.ImportDir(".", build.FindOnly); err != nil || here.Dir == "" {
		panic("build.ImportDir")
	}
	if !build.IsLocalImport("./x") || build.IsLocalImport("fmt") {
		panic("build.IsLocalImport")
	}

	fset := token.NewFileSet()
	file := fset.AddFile("sample.go", -1, 32)
	file.AddLine(8)
	pos := file.Pos(9)
	if got := fset.Position(pos); got.Filename != "sample.go" || got.Line != 2 {
		panic("token.FileSet")
	}
	if token.Lookup("func") != token.FUNC || !token.IsIdentifier("value1") {
		panic("token lookup")
	}
	// Keep importer.ForCompiler with the gc lookup path from #1389.
	if importer.ForCompiler(fset, "gc", nil) == nil {
		panic("importer.ForCompiler")
	}

	typesPkg := types.NewPackage("example.test/tooling", "tooling")
	sig := types.NewSignatureType(nil, nil, nil, nil, nil, false)
	fn := types.NewFunc(token.NoPos, typesPkg, "Run", sig)
	var obj types.Object = &wrappedFunc{Func: fn}
	if old := typesPkg.Scope().Insert(obj); old != nil || typesPkg.Scope().Lookup("Run") != obj {
		panic("types.Scope")
	}
	named := types.NewNamed(types.NewTypeName(token.NoPos, typesPkg, "Count", nil), types.Typ[types.Int], nil)
	if named.Underlying() != types.Typ[types.Int] || !types.ConvertibleTo(named, types.Typ[types.Int]) {
		panic("types.Named")
	}
	println("go tooling ok")
}
