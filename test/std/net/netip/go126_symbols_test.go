//go:build go1.26

package netip_test

import (
	"net/netip"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var prefix netip.Prefix
	_ = prefix.Compare
}
