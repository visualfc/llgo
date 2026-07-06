//go:build !llgo

package ssa

import (
	"go/types"
	"testing"
)

func TestRecoverMetadataHandlesNonFunctions(t *testing.T) {
	prog := NewProgram(nil)
	values := []Expr{
		Nil,
		{Type: prog.Int()},
		prog.Val(1),
	}
	for i, value := range values {
		if got := value.MarkMayRecover(); got.Type != value.Type {
			t.Fatalf("value %d: MarkMayRecover changed the expression type", i)
		}
		if value.mayRecover() {
			t.Fatalf("value %d: non-function marked as recover-capable", i)
		}
	}

	pkg := prog.NewPackage("p", "example.com/p")
	fn := pkg.NewFunc("recovering", NoArgsNoRet, InGo)
	if fn.Expr.mayRecover() {
		t.Fatal("new function unexpectedly marked as recover-capable")
	}
	fn.Expr.MarkMayRecover()
	if !fn.Expr.mayRecover() {
		t.Fatal("marked function is not recover-capable")
	}
	// Marking a function twice must preserve the existing attribute.
	fn.Expr.MarkMayRecover()
	if !fn.Expr.mayRecover() {
		t.Fatal("re-marked function lost recover capability")
	}
}

func TestRecoverDeferClassificationFallbacks(t *testing.T) {
	prog := NewProgram(nil)
	pkg := prog.NewPackage("p", "example.com/p")
	callee := pkg.NewFunc("callee", NoArgsNoRet, InGo)
	caller := pkg.NewFunc("caller", NoArgsNoRet, InGo)
	b := caller.MakeBody(1)

	fnPtr := b.ChangeType(prog.rawType(NoArgsNoRet), callee.Expr)
	ifaceMethod := Expr{Type: &aType{kind: vkIfaceMethod}}
	ordinary := prog.Val(1)

	for _, test := range []struct {
		name string
		fn   Expr
		want bool
	}{
		{name: "nil", fn: Nil, want: false},
		{name: "ordinary", fn: ordinary, want: false},
		{name: "function declaration", fn: callee.Expr, want: false},
		{name: "function pointer", fn: fnPtr, want: true},
		{name: "interface method", fn: ifaceMethod, want: true},
	} {
		t.Run(test.name, func(t *testing.T) {
			if got := deferMayRecover(test.fn); got != test.want {
				t.Fatalf("deferMayRecover() = %v, want %v", got, test.want)
			}
		})
	}

	callee.Expr.MarkMayRecover()
	if !deferMayRecover(callee.Expr) {
		t.Fatal("marked function declaration is not recover-capable")
	}
	if token := b.recoverDeferToken(fnPtr, false); token.IsNil() {
		t.Fatal("function pointer did not produce a recover token")
	}
	if token := b.recoverDeferToken(ordinary, true); !token.IsNil() {
		t.Fatal("ordinary value unexpectedly produced a recover token")
	}

	if !isRecoverBuiltin(Builtin("recover")) {
		t.Fatal("recover builtin not recognized")
	}
	for _, value := range []Expr{
		Nil,
		ordinary,
		Builtin("panic"),
		{Type: &aType{raw: rawType{Type: types.Typ[types.Int]}, kind: vkBuiltin}},
	} {
		if isRecoverBuiltin(value) {
			t.Fatalf("%v unexpectedly recognized as the recover builtin", value.Type)
		}
	}

	b.Return()
	b.EndBuild()
}
