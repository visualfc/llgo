//go:build go1.27

package bytes_test

import (
	"bytes"
	"testing"
)

func TestCutLast(t *testing.T) {
	input := []byte("go+plus+llgo")
	before, after, found := bytes.CutLast(input, []byte("+"))
	if !found || string(before) != "go+plus" || string(after) != "llgo" {
		t.Fatalf("CutLast = %q, %q, %v", before, after, found)
	}
	before[0] = 'G'
	if input[0] != 'G' {
		t.Fatal("CutLast result does not alias its input")
	}
}
