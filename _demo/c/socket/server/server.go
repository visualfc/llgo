package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/net"
)

func main() {
	var buffer [256]c.Char

	sockfd := net.Socket(net.AF_INET, net.SOCK_STREAM, 0)
	if sockfd == net.InvalidSocket {
		panic("socket failed")
	}
	defer net.Close(sockfd)

	servAddr := &net.SockaddrIn{
		Family: net.AF_INET,
		Port:   net.Htons(uint16(1234)),
		Addr:   net.InAddr{Addr: 0x00000000},
		Zero:   [8]c.Char{0, 0, 0, 0, 0, 0, 0, 0},
	}
	if res := net.Bind(sockfd, servAddr, net.SocklenT(unsafe.Sizeof(*servAddr))); res < 0 {
		panic("bind failed")
	}

	if net.Listen(sockfd, 5) < 0 {
		panic("listen failed")
	}
	c.Printf(c.Str("Listening on port 1234...\n"))
	c.Fflush(c.Stdout)

	cliAddr := &net.SockaddrIn{}
	clilen := net.SocklenT(unsafe.Sizeof(*cliAddr))

	newsockfd := net.Accept(sockfd, cliAddr, &clilen)
	if newsockfd == net.InvalidSocket {
		panic("accept failed")
	}
	defer net.Close(newsockfd)
	count := net.Recv(newsockfd, unsafe.Pointer(unsafe.SliceData(buffer[:])), uintptr(len(buffer)-1), 0)
	if count <= 0 {
		panic("receive failed")
	}
	buffer[int(count)] = 0
	c.Printf(c.Str("Connection accepted.\nReceived: %s\n"), &buffer[0])
	c.Fflush(c.Stdout)
}
