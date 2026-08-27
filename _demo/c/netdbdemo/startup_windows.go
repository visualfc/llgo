//go:build windows

package main

import "github.com/goplus/lib/c/net"

func startupNetwork() func() {
	var data net.WSAData
	if result := net.WSAStartup(net.MakeWord(2, 2), &data); result != 0 {
		panic("WSAStartup failed")
	}
	return func() {
		if result := net.WSACleanup(); result != 0 {
			panic("WSACleanup failed")
		}
	}
}
