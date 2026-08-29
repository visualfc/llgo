package main

import (
	"go/build"
	"go/importer"
	"go/token"
	"go/types"
)

// wrappedFunc preserves the embedded-unexported-object regression (#1598).
type wrappedFunc struct{ *types.Func }

// This case keeps the former go/build, go/token, and go/types API matrices in
// separate source files while compiling and linking them as one command.
func main() {
	testBuildAPI()
	testTokenAPI()
	testTypesAPI()

	// Keep the exact address-of-global build regression (#1389).
	ctx := &build.Default
	if ctx.Compiler == "" || ctx.GOOS == "" || ctx.GOARCH == "" {
		panic("incomplete build.Default")
	}

	fset := token.NewFileSet()
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
	println("go tooling ok")
}
