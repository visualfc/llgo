//go:build go1.27

package strings_test

import (
	"strings"
	"testing"
)

func TestCutLast(t *testing.T) {
	before, after, found := strings.CutLast("go+plus+llgo", "+")
	if !found || before != "go+plus" || after != "llgo" {
		t.Fatalf("CutLast = %q, %q, %v", before, after, found)
	}
}
