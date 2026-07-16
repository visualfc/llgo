//go:build go1.26

package ecdsa_test

import (
	"crypto/ecdsa"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var privateKey ecdsa.PrivateKey
	var publicKey ecdsa.PublicKey
	_ = privateKey.Bytes
	_ = publicKey.Bytes
}
