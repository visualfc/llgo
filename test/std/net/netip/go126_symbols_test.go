//go:build go1.26

package netip_test

import (
	"net/netip"
	"testing"
)

func TestPrefixCompare(t *testing.T) {
	short := netip.MustParsePrefix("192.0.2.0/24")
	long := netip.MustParsePrefix("192.0.2.0/25")
	next := netip.MustParsePrefix("192.0.3.0/24")
	if got := short.Compare(long); got >= 0 {
		t.Fatalf("short.Compare(long) = %d, want negative", got)
	}
	if got := long.Compare(short); got <= 0 {
		t.Fatalf("long.Compare(short) = %d, want positive", got)
	}
	if got := short.Compare(short); got != 0 {
		t.Fatalf("short.Compare(short) = %d, want zero", got)
	}
	if got := short.Compare(next); got >= 0 {
		t.Fatalf("short.Compare(next) = %d, want negative", got)
	}
}
