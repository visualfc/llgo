//go:build go1.27

package mldsa_test

import (
	"crypto/mldsa"
	"testing"
)

func TestKeyEncodingAndSignature(t *testing.T) {
	params := mldsa.MLDSA44()
	seed := make([]byte, mldsa.PrivateKeySize)
	for i := range seed {
		seed[i] = byte(i)
	}
	privateKey, err := mldsa.NewPrivateKey(params, seed)
	if err != nil {
		t.Fatal(err)
	}
	publicKey, err := mldsa.NewPublicKey(params, privateKey.PublicKey().Bytes())
	if err != nil {
		t.Fatal(err)
	}
	message := []byte("llgo mldsa")
	signature, err := privateKey.SignDeterministic(message, nil)
	if err != nil {
		t.Fatal(err)
	}
	if err := mldsa.Verify(publicKey, message, signature, nil); err != nil {
		t.Fatalf("Verify failed: %v", err)
	}
	if err := mldsa.Verify(publicKey, []byte("changed"), signature, nil); err == nil {
		t.Fatal("Verify accepted altered message")
	}
}
