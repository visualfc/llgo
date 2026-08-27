//go:build !windows

package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/net"
	"github.com/goplus/lib/c/os"
)

type socketHandle = c.Int

func startupSockets() func() { return func() {} }

func openSocket() socketHandle { return net.Socket(net.AF_INET, net.SOCK_STREAM, 0) }

func socketFailed(socket socketHandle) bool { return socket < 0 }

func closeSocket(socket socketHandle) { os.Close(socket) }

func connectSocket(socket socketHandle, address *net.SockAddr, size uintptr) c.Int {
	return net.Connect(socket, address, c.Uint(size))
}

func sendSocket(socket socketHandle, buffer c.Pointer, length uintptr) int64 {
	return int64(net.Send(socket, buffer, length, 0))
}

func socketError() int { return int(os.Errno()) }
