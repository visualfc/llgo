//go:build windows

package main

import (
	"unsafe"

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

func bindSocket(socket socketHandle, address *net.SockaddrIn) c.Int {
	return net.Bind(socket, (*net.SockAddr)(unsafe.Pointer(address)), net.SocklenT(unsafe.Sizeof(*address)))
}

func listenSocket(socket socketHandle, backlog c.Int) c.Int {
	return net.Listen(socket, backlog)
}

func acceptSocket(socket socketHandle) socketHandle {
	address := new(net.SockaddrIn)
	length := net.SocklenT(unsafe.Sizeof(*address))
	return net.Accept(socket, (*net.SockAddr)(unsafe.Pointer(address)), &length)
}

func recvSocket(socket socketHandle, buffer c.Pointer, length uintptr) int64 {
	return int64(net.Recv(socket, buffer, c.Int(length), 0))
}
