//go:build go1.26

package crypto_test

import (
	"crypto"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var _ crypto.Decapsulator
	var _ crypto.Encapsulator
	var _ crypto.MessageSigner
	_ = crypto.SignMessage
}
