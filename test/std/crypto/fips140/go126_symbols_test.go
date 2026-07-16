//go:build go1.26

package fips140_test

import (
	"crypto/fips140"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	_ = fips140.Enforced
	_ = fips140.Version
	_ = fips140.WithoutEnforcement
}
