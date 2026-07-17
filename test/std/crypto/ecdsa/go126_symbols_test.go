//go:build go1.26

package ecdsa_test

import (
	"bytes"
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"testing"
)

func TestKeyBytes(t *testing.T) {
	privateKey, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		t.Fatal(err)
	}

	privateBytes, err := privateKey.Bytes()
	if err != nil {
		t.Fatal(err)
	}
	if len(privateBytes) != 32 {
		t.Fatalf("PrivateKey.Bytes length = %d, want 32", len(privateBytes))
	}

	publicBytes, err := privateKey.PublicKey.Bytes()
	if err != nil {
		t.Fatal(err)
	}
	wantPublic := elliptic.Marshal(elliptic.P256(), privateKey.X, privateKey.Y)
	if !bytes.Equal(publicBytes, wantPublic) {
		t.Fatalf("PublicKey.Bytes = %x, want %x", publicBytes, wantPublic)
	}
}
