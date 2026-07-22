//go:build go1.26

package fips140_test

import (
	"crypto/fips140"
	"testing"
)

func TestEnforcementInformation(t *testing.T) {
	if version := fips140.Version(); version == "" {
		t.Fatal("Version returned an empty string")
	}

	outer := fips140.Enforced()
	called := false
	fips140.WithoutEnforcement(func() {
		called = true
		if fips140.Enforced() {
			t.Fatal("WithoutEnforcement did not disable enforcement")
		}
	})
	if !called {
		t.Fatal("WithoutEnforcement did not call its function")
	}
	if fips140.Enforced() != outer {
		t.Fatal("WithoutEnforcement did not restore enforcement state")
	}
}
