//go:build go1.26

package sha3_test

import (
	"crypto/sha3"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var hash sha3.SHA3
	_ = hash.Clone
}
