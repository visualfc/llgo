//go:build go1.26

package types_test

import (
	"go/types"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var variable *types.Var
	var kind types.VarKind
	_ = variable.Kind
	_ = variable.SetKind
	_ = kind.String
}
