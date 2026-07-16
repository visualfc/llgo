//go:build go1.26

package http_test

import (
	"net/http"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var connection http.ClientConn
	var protection http.CrossOriginProtection
	var transport http.Transport
	_ = connection.Available
	_ = connection.Close
	_ = connection.Err
	_ = connection.InFlight
	_ = connection.Release
	_ = connection.Reserve
	_ = connection.RoundTrip
	_ = connection.SetStateHook
	_ = protection.AddInsecureBypassPattern
	_ = protection.AddTrustedOrigin
	_ = protection.Check
	_ = protection.Handler
	_ = protection.SetDenyHandler
	_ = transport.NewClientConn
}
