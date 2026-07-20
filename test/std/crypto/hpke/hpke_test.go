//go:build go1.26

package hpke_test

import (
	"bytes"
	"crypto/ecdh"
	"crypto/hpke"
	"crypto/rand"
	"testing"
)

func TestSealOpen(t *testing.T) {
	private, err := ecdh.P256().GenerateKey(rand.Reader)
	if err != nil {
		t.Fatal(err)
	}
	publicKey, err := hpke.NewDHKEMPublicKey(private.PublicKey())
	if err != nil {
		t.Fatal(err)
	}
	privateKey, err := hpke.NewDHKEMPrivateKey(private)
	if err != nil {
		t.Fatal(err)
	}
	info := []byte("llgo hpke")
	plaintext := []byte("standard library")
	ciphertext, err := hpke.Seal(publicKey, hpke.HKDFSHA256(), hpke.AES128GCM(), info, plaintext)
	if err != nil {
		t.Fatal(err)
	}
	got, err := hpke.Open(privateKey, hpke.HKDFSHA256(), hpke.AES128GCM(), info, ciphertext)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(got, plaintext) {
		t.Fatalf("Open = %q, want %q", got, plaintext)
	}
	if _, err := hpke.Open(privateKey, hpke.HKDFSHA256(), hpke.AES128GCM(), []byte("wrong info"), ciphertext); err == nil {
		t.Fatal("Open with different info unexpectedly succeeded")
	}
}

func TestSenderRecipient(t *testing.T) {
	private, err := ecdh.P256().GenerateKey(rand.Reader)
	if err != nil {
		t.Fatal(err)
	}
	publicKey, err := hpke.NewDHKEMPublicKey(private.PublicKey())
	if err != nil {
		t.Fatal(err)
	}
	privateKey, err := hpke.NewDHKEMPrivateKey(private)
	if err != nil {
		t.Fatal(err)
	}
	encapsulation, sender, err := hpke.NewSender(publicKey, hpke.HKDFSHA256(), hpke.AES128GCM(), []byte("info"))
	if err != nil {
		t.Fatal(err)
	}
	recipient, err := hpke.NewRecipient(encapsulation, privateKey, hpke.HKDFSHA256(), hpke.AES128GCM(), []byte("info"))
	if err != nil {
		t.Fatal(err)
	}
	ciphertext, err := sender.Seal([]byte("aad"), []byte("message"))
	if err != nil {
		t.Fatal(err)
	}
	plaintext, err := recipient.Open([]byte("aad"), ciphertext)
	if err != nil || string(plaintext) != "message" {
		t.Fatalf("Open = (%q, %v)", plaintext, err)
	}
	senderExport, err := sender.Export("context", 32)
	if err != nil {
		t.Fatal(err)
	}
	recipientExport, err := recipient.Export("context", 32)
	if err != nil || !bytes.Equal(senderExport, recipientExport) {
		t.Fatal("sender and recipient exports differ")
	}
}

func TestComponentInterfaces(t *testing.T) {
	var kem hpke.KEM = hpke.DHKEM(ecdh.P256())
	if kem.ID() == 0 {
		t.Fatal("DHKEM returned a zero KEM identifier")
	}
	private, err := kem.GenerateKey()
	if err != nil {
		t.Fatal(err)
	}
	var privateKey hpke.PrivateKey = private
	privateBytes, err := privateKey.Bytes()
	if err != nil || len(privateBytes) == 0 {
		t.Fatalf("private key serialization = %d bytes, %v", len(privateBytes), err)
	}
	if privateKey.KEM().ID() != kem.ID() {
		t.Fatal("private key reports a different KEM")
	}

	var publicKey hpke.PublicKey = privateKey.PublicKey()
	if len(publicKey.Bytes()) == 0 || publicKey.KEM().ID() != kem.ID() {
		t.Fatal("public key serialization or KEM identity is invalid")
	}

	var kdf hpke.KDF = hpke.HKDFSHA256()
	var aead hpke.AEAD = hpke.AES128GCM()
	if kdf.ID() == 0 || aead.ID() == 0 {
		t.Fatalf("invalid ciphersuite identifiers: KDF=%d AEAD=%d", kdf.ID(), aead.ID())
	}
}
