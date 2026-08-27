//go:build windows

package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/net"
)

type socketHandle = net.SocketT

func startupSockets() func() {
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

func openSocket() socketHandle { return net.Socket(net.AF_INET, net.SOCK_STREAM, 0) }

func socketFailed(socket socketHandle) bool { return socket == net.InvalidSocket }

func closeSocket(socket socketHandle) { net.Closesocket(socket) }

func connectSocket(socket socketHandle, address *net.SockAddr, size uintptr) c.Int {
	return net.Connect(socket, address, net.SocklenT(size))
}

func sendSocket(socket socketHandle, buffer c.Pointer, length uintptr) int64 {
	return int64(net.Send(socket, buffer, c.Int(length), 0))
}

func socketError() int { return int(net.WSAGetLastError()) }
