//go:build go1.26

package types_test

import (
	"go/token"
	"go/types"
	"testing"
)

func TestVarKind(t *testing.T) {
	variable := types.NewVar(token.NoPos, nil, "value", types.Typ[types.Int])
	if got := variable.Kind(); got != types.PackageVar {
		t.Fatalf("new variable kind = %v, want %v", got, types.PackageVar)
	}
	variable.SetKind(types.LocalVar)
	if got := variable.Kind(); got != types.LocalVar {
		t.Fatalf("updated variable kind = %v, want %v", got, types.LocalVar)
	}
	if got := types.LocalVar.String(); got != "LocalVar" {
		t.Fatalf("LocalVar.String = %q", got)
	}
}
