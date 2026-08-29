//go:build go1.27

package maphash_test

import (
	"hash/maphash"
	"testing"
)

func TestComparableHasher(t *testing.T) {
	hasher := maphash.ComparableHasher[string]{}
	var _ maphash.Hasher[string] = hasher
	if !hasher.Equal("llgo", "llgo") || hasher.Equal("llgo", "go") {
		t.Fatal("ComparableHasher.Equal does not match ==")
	}
	var left, right maphash.Hash
	seed := maphash.MakeSeed()
	left.SetSeed(seed)
	right.SetSeed(seed)
	hasher.Hash(&left, "llgo")
	hasher.Hash(&right, "llgo")
	if left.Sum64() != right.Sum64() {
		t.Fatal("ComparableHasher.Hash is not deterministic for one seed")
	}
}
