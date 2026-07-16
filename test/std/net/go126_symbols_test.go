//go:build go1.26

package net_test

import (
	"net"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var dialer net.Dialer
	_ = dialer.DialIP
	_ = dialer.DialTCP
	_ = dialer.DialUDP
	_ = dialer.DialUnix
}
