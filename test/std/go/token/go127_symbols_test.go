//go:build go1.27

package token_test

import (
	"go/token"
	"testing"
)

func TestFileString(t *testing.T) {
	file := token.NewFileSet().AddFile("source.go", 10, 20)
	if got, want := file.String(), "source.go(10-30)"; got != want {
		t.Fatalf("File.String = %q, want %q", got, want)
	}
}
