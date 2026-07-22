//go:build go1.26

package x509_test

import (
	"crypto/x509"
	"testing"
)

func TestUsageFormatting(t *testing.T) {
	usage := x509.ExtKeyUsageServerAuth
	if got := usage.OID().String(); got != "1.3.6.1.5.5.7.3.1" {
		t.Fatalf("ExtKeyUsageServerAuth.OID = %q", got)
	}
	if got := usage.String(); got != "serverAuth" {
		t.Fatalf("ExtKeyUsageServerAuth.String = %q", got)
	}
	if got := x509.KeyUsageDigitalSignature.String(); got != "digitalSignature" {
		t.Fatalf("KeyUsageDigitalSignature.String = %q", got)
	}
}
