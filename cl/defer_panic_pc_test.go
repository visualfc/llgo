//go:build !llgo
// +build !llgo

package cl

import (
	"fmt"
	"go/ast"
	"go/token"
	"strings"
	"testing"

	"golang.org/x/tools/go/ssa"
)

func TestDeferRunPos(t *testing.T) {
	ssapkg, _ := buildCallerFrameSSAPackage(t, "example.com/foo", `package foo

func declared() {}

func outer() {
	func() {}()
}
`)
	fallback := token.Pos(1)

	declFn := ssapkg.Func("declared")
	decl, ok := declFn.Syntax().(*ast.FuncDecl)
	if !ok {
		t.Fatalf("declared syntax = %T, want *ast.FuncDecl", declFn.Syntax())
	}
	ctx := &context{goFn: declFn}
	if got := ctx.deferRunPos(fallback); got != decl.Body.Rbrace {
		t.Fatalf("declaration defer position = %v, want closing brace %v", got, decl.Body.Rbrace)
	}

	outer := ssapkg.Func("outer")
	if len(outer.AnonFuncs) != 1 {
		t.Fatalf("outer anonymous functions = %d, want 1", len(outer.AnonFuncs))
	}
	litFn := outer.AnonFuncs[0]
	lit, ok := litFn.Syntax().(*ast.FuncLit)
	if !ok {
		t.Fatalf("literal syntax = %T, want *ast.FuncLit", litFn.Syntax())
	}
	ctx.goFn = litFn
	if got := ctx.deferRunPos(fallback); got != lit.Body.Rbrace {
		t.Fatalf("literal defer position = %v, want closing brace %v", got, lit.Body.Rbrace)
	}

	declBody := decl.Body
	decl.Body = nil
	ctx.goFn = declFn
	if got := ctx.deferRunPos(fallback); got != fallback {
		t.Fatalf("bodyless declaration position = %v, want fallback %v", got, fallback)
	}
	decl.Body = declBody
	declRbrace := decl.Body.Rbrace
	decl.Body.Rbrace = token.NoPos
	if got := ctx.deferRunPos(fallback); got != fallback {
		t.Fatalf("declaration with invalid closing brace position = %v, want fallback %v", got, fallback)
	}
	decl.Body.Rbrace = declRbrace

	litBody := lit.Body
	lit.Body = nil
	ctx.goFn = litFn
	if got := ctx.deferRunPos(fallback); got != fallback {
		t.Fatalf("bodyless literal position = %v, want fallback %v", got, fallback)
	}
	lit.Body = litBody
	litRbrace := lit.Body.Rbrace
	lit.Body.Rbrace = token.NoPos
	if got := ctx.deferRunPos(fallback); got != fallback {
		t.Fatalf("literal with invalid closing brace position = %v, want fallback %v", got, fallback)
	}
	lit.Body.Rbrace = litRbrace

	ctx.goFn = &ssa.Function{}
	if got := ctx.deferRunPos(fallback); got != fallback {
		t.Fatalf("function without syntax position = %v, want fallback %v", got, fallback)
	}
	ctx.goFn = nil
	if got := ctx.deferRunPos(fallback); got != fallback {
		t.Fatalf("nil function position = %v, want fallback %v", got, fallback)
	}
}

func TestCompileDeferAndPanicPCLineAnchors(t *testing.T) {
	ssapkg, files := buildCallerFrameSSAPackage(t, "example.com/foo", `package foo
import "runtime"

func withDefer() {
	defer func() {}()
	runtime.Caller(0)
}

func withLiteralDefer() {
	func() {
		defer func() {}()
		runtime.Caller(0)
	}()
}

func withPanic() {
	runtime.Caller(0)
	panic("boom")
}
`)
	prog := newLLSSAProg(t)
	prog.Target().GOOS = "linux"
	prog.Target().GOARCH = "amd64"
	prog.EnableFuncInfoMetadata(true)
	prog.EnableFuncInfoSites(true)
	pkg, err := NewPackage(prog, ssapkg, files)
	if err != nil {
		t.Fatal(err)
	}
	ir := pkg.Module().String()

	tests := []struct {
		symbol string
		line   int
		column int
	}{
		{symbol: "example.com/foo.withDefer", line: 7, column: 1},
		{symbol: "example.com/foo.withLiteralDefer$1", line: 13, column: 2},
		{symbol: "example.com/foo.withPanic", line: 18, column: 7},
	}
	for _, tt := range tests {
		if !hasPCLineMetadataPosition(ir, tt.symbol, tt.line, tt.column) {
			t.Fatalf("missing pc-line anchor for %s at caller_frame_compile.go:%d:%d:\n%s", tt.symbol, tt.line, tt.column, ir)
		}
	}
}

func hasPCLineMetadataPosition(ir, symbol string, line, column int) bool {
	want := fmt.Sprintf(`!%q, !"caller_frame_compile.go", i32 %d, i32 %d}`, symbol, line, column)
	for _, row := range strings.Split(ir, "\n") {
		if strings.Contains(row, "= !{i32 1, i64 ") && strings.Contains(row, want) {
			return true
		}
	}
	return false
}
