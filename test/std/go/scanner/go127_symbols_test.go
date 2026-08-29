//go:build go1.27

package scanner_test

import (
	"go/scanner"
	"go/token"
	"testing"
)

func TestScannerEnd(t *testing.T) {
	const source = "name"
	files := token.NewFileSet()
	file := files.AddFile("source.go", files.Base(), len(source))
	var scan scanner.Scanner
	scan.Init(file, []byte(source), nil, 0)
	if got := scan.End(); got != token.NoPos {
		t.Fatalf("End before Scan = %d, want NoPos", got)
	}
	pos, kind, literal := scan.Scan()
	if kind != token.IDENT || literal != source {
		t.Fatalf("Scan = %d, %v, %q", pos, kind, literal)
	}
	if got, want := scan.End(), file.Pos(len(source)); got != want {
		t.Fatalf("End = %d, want %d", got, want)
	}
}
