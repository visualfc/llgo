//go:build go1.26

package hash_test

import (
	"hash"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var _ hash.Cloner
	var _ hash.XOF
}
