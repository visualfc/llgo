//go:build go1.26

package x509_test

import (
	"crypto/x509"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var extendedKeyUsage x509.ExtKeyUsage
	var keyUsage x509.KeyUsage
	_ = extendedKeyUsage.OID
	_ = extendedKeyUsage.String
	_ = keyUsage.String
}
