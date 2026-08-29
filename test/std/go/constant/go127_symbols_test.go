//go:build go1.27

package constant_test

import (
	"go/constant"
	"testing"
)

func TestStringLen(t *testing.T) {
	if got := constant.StringLen(constant.MakeString("世界")); got != 6 {
		t.Fatalf("StringLen = %d, want 6", got)
	}
	if got := constant.StringLen(constant.MakeUnknown()); got != 0 {
		t.Fatalf("StringLen(unknown) = %d, want 0", got)
	}
}
