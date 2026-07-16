//go:build go1.26

package net_test

import (
	"context"
	"net"
	"net/netip"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestDialerTypedNetworkMethods(t *testing.T) {
	t.Run("TCP", func(t *testing.T) {
		listener, err := net.ListenTCP("tcp4", &net.TCPAddr{IP: net.IPv4(127, 0, 0, 1)})
		if err != nil {
			t.Fatal(err)
		}
		defer listener.Close()

		accepted := make(chan error, 1)
		go func() {
			connection, err := listener.AcceptTCP()
			if err == nil {
				defer connection.Close()
				buffer := make([]byte, 4)
				_, err = connection.Read(buffer)
				if err == nil && string(buffer) != "ping" {
					err = &net.AddrError{Err: "unexpected TCP payload", Addr: string(buffer)}
				}
			}
			accepted <- err
		}()

		remote := listener.Addr().(*net.TCPAddr).AddrPort()
		connection, err := new(net.Dialer).DialTCP(t.Context(), "tcp4", netip.AddrPort{}, remote)
		if err != nil {
			t.Fatal(err)
		}
		if _, err := connection.Write([]byte("ping")); err != nil {
			t.Fatal(err)
		}
		connection.Close()
		if err := <-accepted; err != nil {
			t.Fatal(err)
		}
	})

	t.Run("UDP", func(t *testing.T) {
		listener, err := net.ListenUDP("udp4", &net.UDPAddr{IP: net.IPv4(127, 0, 0, 1)})
		if err != nil {
			t.Fatal(err)
		}
		defer listener.Close()
		listener.SetReadDeadline(time.Now().Add(5 * time.Second))

		remote := listener.LocalAddr().(*net.UDPAddr).AddrPort()
		connection, err := new(net.Dialer).DialUDP(t.Context(), "udp4", netip.AddrPort{}, remote)
		if err != nil {
			t.Fatal(err)
		}
		defer connection.Close()
		if _, err := connection.Write([]byte("datagram")); err != nil {
			t.Fatal(err)
		}
		buffer := make([]byte, 16)
		n, _, err := listener.ReadFromUDP(buffer)
		if err != nil || string(buffer[:n]) != "datagram" {
			t.Fatalf("ReadFromUDP = %q, %v; want datagram, nil", buffer[:n], err)
		}
	})

	t.Run("Unix", func(t *testing.T) {
		directory, err := os.MkdirTemp("/tmp", "llgo-net-")
		if err != nil {
			t.Fatal(err)
		}
		defer os.RemoveAll(directory)
		path := filepath.Join(directory, "listener.sock")
		address := &net.UnixAddr{Name: path, Net: "unix"}
		listener, err := net.ListenUnix("unix", address)
		if err != nil {
			t.Fatal(err)
		}
		defer listener.Close()

		accepted := make(chan error, 1)
		go func() {
			connection, err := listener.AcceptUnix()
			if err == nil {
				connection.Close()
			}
			accepted <- err
		}()
		connection, err := new(net.Dialer).DialUnix(t.Context(), "unix", nil, address)
		if err != nil {
			t.Fatal(err)
		}
		connection.Close()
		if err := <-accepted; err != nil {
			t.Fatal(err)
		}
	})

	t.Run("IPError", func(t *testing.T) {
		ctx, cancel := context.WithCancel(t.Context())
		cancel()
		connection, err := new(net.Dialer).DialIP(ctx, "not-an-ip-network", netip.Addr{}, netip.Addr{})
		if connection != nil {
			connection.Close()
			t.Fatal("DialIP returned a connection for an invalid network")
		}
		if err == nil || (!strings.Contains(err.Error(), "unknown network") && !strings.Contains(err.Error(), "canceled")) {
			t.Fatalf("DialIP error = %v, want an invalid-network or canceled error", err)
		}
	})
}
