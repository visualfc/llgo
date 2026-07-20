//go:build go1.26

package cryptotest_test

import (
	"bytes"
	"crypto/rand"
	"testing"
	"testing/cryptotest"
)

func TestSetGlobalRandom(t *testing.T) {
	cryptotest.SetGlobalRandom(t, 1)
	first := make([]byte, 32)
	if _, err := rand.Read(first); err != nil {
		t.Fatal(err)
	}
	cryptotest.SetGlobalRandom(t, 1)
	second := make([]byte, 32)
	if _, err := rand.Read(second); err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(first, second) {
		t.Fatal("resetting the seed did not reproduce the random stream")
	}
}
