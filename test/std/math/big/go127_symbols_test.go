//go:build go1.27

package big_test

import (
	"math/big"
	"testing"
)

func TestIntDivideRoundingModes(t *testing.T) {
	tests := []struct {
		mode      big.RoundingMode
		quotient  int64
		remainder int64
	}{
		{big.Trunc, -3, -1},
		{big.Floor, -4, 1},
		{big.Round, -4, 1},
		{big.Ceil, -3, -1},
	}
	for _, test := range tests {
		quotient, remainder := new(big.Int), new(big.Int)
		gotQuotient, gotRemainder := quotient.Divide(big.NewInt(-7), big.NewInt(2), remainder, test.mode)
		if gotQuotient != quotient || gotRemainder != remainder {
			t.Fatalf("Divide did not return its result arguments for %v", test.mode)
		}
		if quotient.Int64() != test.quotient || remainder.Int64() != test.remainder {
			t.Fatalf("Divide(%v) = %s remainder %s", test.mode, quotient, remainder)
		}
	}
}
