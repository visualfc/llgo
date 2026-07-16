//go:build go1.26

package rsa_test

import (
	"crypto/rsa"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	_ = rsa.EncryptOAEPWithOptions
}
