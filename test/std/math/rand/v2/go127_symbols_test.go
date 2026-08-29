//go:build go1.27

package randv2_test

import (
	"math/rand/v2"
	"testing"
)

func TestRandNPreservesIntegerType(t *testing.T) {
	random := rand.New(rand.NewPCG(1, 2))
	got := random.N(uint16(10))
	if got >= 10 {
		t.Fatalf("Rand.N = %d, want [0, 10)", got)
	}
	var _ uint16 = got
}
