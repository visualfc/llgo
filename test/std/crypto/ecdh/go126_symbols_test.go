//go:build go1.26

package ecdh_test

import (
	"crypto/ecdh"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var _ ecdh.KeyExchanger
}
