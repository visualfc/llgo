//go:build go1.26

package unicode_test

import (
	"testing"
	"unicode"
)

func TestGo126Symbols(t *testing.T) {
	_ = unicode.CategoryAliases
	_ = unicode.Cn
	_ = unicode.LC
}
