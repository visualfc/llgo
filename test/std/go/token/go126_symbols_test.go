//go:build go1.26

package token_test

import (
	"go/token"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var file token.File
	var fileSet token.FileSet
	_ = file.End
	_ = fileSet.AddExistingFiles
}
