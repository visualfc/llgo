//go:build go1.26

package maphash_test

import (
	"hash/maphash"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var hash maphash.Hash
	_ = hash.Clone
}
