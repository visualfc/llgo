//go:build go1.26

package mlkem_test

import (
	"bytes"
	"crypto"
	"crypto/mlkem"
	"testing"
)

func testEncapsulator(t *testing.T, key crypto.Decapsulator, encapsulator crypto.Encapsulator) {
	t.Helper()
	shared, ciphertext := encapsulator.Encapsulate()
	got, err := key.Decapsulate(ciphertext)
	if err != nil {
		t.Fatal(err)
	}
	if len(encapsulator.Bytes()) == 0 || len(ciphertext) == 0 || !bytes.Equal(got, shared) {
		t.Fatal("ML-KEM encapsulation round trip failed")
	}
}

func TestEncapsulator(t *testing.T) {
	key768, err := mlkem.GenerateKey768()
	if err != nil {
		t.Fatal(err)
	}
	testEncapsulator(t, key768, key768.Encapsulator())

	key1024, err := mlkem.GenerateKey1024()
	if err != nil {
		t.Fatal(err)
	}
	testEncapsulator(t, key1024, key1024.Encapsulator())
}
