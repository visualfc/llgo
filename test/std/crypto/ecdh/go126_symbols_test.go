//go:build go1.26

package ecdh_test

import (
	"bytes"
	"crypto/ecdh"
	"crypto/rand"
	"testing"
)

func TestKeyExchanger(t *testing.T) {
	aliceKey, err := ecdh.X25519().GenerateKey(rand.Reader)
	if err != nil {
		t.Fatal(err)
	}
	bobKey, err := ecdh.X25519().GenerateKey(rand.Reader)
	if err != nil {
		t.Fatal(err)
	}

	var alice, bob ecdh.KeyExchanger = aliceKey, bobKey
	if alice.Curve() != ecdh.X25519() || alice.PublicKey().Curve() != ecdh.X25519() {
		t.Fatal("KeyExchanger returned the wrong curve")
	}
	aliceShared, err := alice.ECDH(bob.PublicKey())
	if err != nil {
		t.Fatal(err)
	}
	bobShared, err := bob.ECDH(alice.PublicKey())
	if err != nil {
		t.Fatal(err)
	}
	if len(aliceShared) == 0 || !bytes.Equal(aliceShared, bobShared) {
		t.Fatalf("ECDH shared keys differ: %x != %x", aliceShared, bobShared)
	}
}
