//go:build go1.26

package ast_test

import (
	"go/ast"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var directive ast.Directive
	var _ ast.DirectiveArg
	_ = directive.End
	_ = directive.ParseArgs
	_ = directive.Pos
	_ = ast.PreorderStack
}
