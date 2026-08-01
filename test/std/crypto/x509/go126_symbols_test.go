//go:build go1.26

package x509_test

import (
	"crypto/x509"
	"encoding/asn1"
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

func TestOIDFromASN1OID(t *testing.T) {
	oid, err := x509.OIDFromASN1OID(asn1.ObjectIdentifier{1, 2, 840, 113549})
	if err != nil {
		t.Fatal(err)
	}
	if got := oid.String(); got != "1.2.840.113549" {
		t.Fatalf("OID.String = %q", got)
	}
	if _, err := x509.OIDFromASN1OID(asn1.ObjectIdentifier{1, 40}); err == nil {
		t.Fatal("OIDFromASN1OID accepted an invalid second arc")
	}
}
