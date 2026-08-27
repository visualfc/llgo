package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/net"
)

func main() {
	var buffer [256]c.Char
	cleanup := startupSockets()
	defer cleanup()

	sockfd := openSocket()
	if socketFailed(sockfd) {
		panic("socket failed")
	}
	defer closeSocket(sockfd)

	servAddr := &net.SockaddrIn{
		Family: net.AF_INET,
		Port:   net.Htons(uint16(1234)),
		Addr:   net.InAddr{Addr: 0x00000000},
		Zero:   [8]c.Char{0, 0, 0, 0, 0, 0, 0, 0},
	}
	if res := bindSocket(sockfd, servAddr); res < 0 {
		panic("bind failed")
	}

	if listenSocket(sockfd, 5) < 0 {
		panic("listen failed")
	}
	c.Printf(c.Str("Listening on port 1234...\n"))
	c.Fflush(c.Stdout)

	newsockfd := acceptSocket(sockfd)
	if socketFailed(newsockfd) {
		panic("accept failed")
	}
	defer closeSocket(newsockfd)
	count := recvSocket(newsockfd, unsafe.Pointer(unsafe.SliceData(buffer[:])), uintptr(len(buffer)-1))
	if count <= 0 {
		panic("receive failed")
	}
	buffer[int(count)] = 0
	c.Printf(c.Str("Connection accepted.\nReceived: %s\n"), &buffer[0])
	c.Fflush(c.Stdout)
}
