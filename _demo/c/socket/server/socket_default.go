//go:build !windows

package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/net"
	"github.com/goplus/lib/c/os"
)

type socketHandle = c.Int

func startupSockets() func() { return func() {} }

func openSocket() socketHandle { return net.Socket(net.AF_INET, net.SOCK_STREAM, 0) }

func socketFailed(socket socketHandle) bool { return socket < 0 }

func closeSocket(socket socketHandle) { os.Close(socket) }

func bindSocket(socket socketHandle, address *net.SockaddrIn) c.Int {
	return net.Bind(socket, address, c.Uint(unsafe.Sizeof(*address)))
}

func listenSocket(socket socketHandle, backlog c.Int) c.Int {
	return net.Listen(socket, backlog)
}

func acceptSocket(socket socketHandle) socketHandle {
	address := new(net.SockaddrIn)
	length := c.Uint(unsafe.Sizeof(*address))
	return net.Accept(socket, address, &length)
}

func recvSocket(socket socketHandle, buffer c.Pointer, length uintptr) int64 {
	return int64(net.Recv(socket, buffer, length, 0))
}
