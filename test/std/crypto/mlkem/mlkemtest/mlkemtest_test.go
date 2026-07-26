//go:build go1.26

package mlkemtest_test

import (
	"bytes"
	"crypto/mlkem"
	"crypto/mlkem/mlkemtest"
	"testing"
)

func TestDeterministicEncapsulation(t *testing.T) {
	key, err := mlkem.NewDecapsulationKey768(make([]byte, mlkem.SeedSize))
	if err != nil {
		t.Fatal(err)
	}
	shared, ciphertext, err := mlkemtest.Encapsulate768(key.EncapsulationKey(), make([]byte, 32))
	if err != nil {
		t.Fatal(err)
	}
	decapsulated, err := key.Decapsulate(ciphertext)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(shared, decapsulated) {
		t.Fatal("encapsulated and decapsulated keys differ")
	}
	repeatedShared, repeatedCiphertext, err := mlkemtest.Encapsulate768(key.EncapsulationKey(), make([]byte, 32))
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(repeatedShared, shared) || !bytes.Equal(repeatedCiphertext, ciphertext) {
		t.Fatal("Encapsulate768 is not deterministic for fixed randomness")
	}
}

func TestDeterministicEncapsulation1024(t *testing.T) {
	key, err := mlkem.NewDecapsulationKey1024(make([]byte, mlkem.SeedSize))
	if err != nil {
		t.Fatal(err)
	}
	shared, ciphertext, err := mlkemtest.Encapsulate1024(key.EncapsulationKey(), make([]byte, 32))
	if err != nil {
		t.Fatal(err)
	}
	decapsulated, err := key.Decapsulate(ciphertext)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(shared, decapsulated) {
		t.Fatal("encapsulated and decapsulated keys differ")
	}
	repeatedShared, repeatedCiphertext, err := mlkemtest.Encapsulate1024(key.EncapsulationKey(), make([]byte, 32))
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(repeatedShared, shared) || !bytes.Equal(repeatedCiphertext, ciphertext) {
		t.Fatal("Encapsulate1024 is not deterministic for fixed randomness")
	}
}
