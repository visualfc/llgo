package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/net"
)

func main() {
	cleanup := startupSockets()
	defer cleanup()

	sockfd := openSocket()
	if socketFailed(sockfd) {
		panic("socket failed")
	}
	msg := c.Str("Hello, World!")
	defer closeSocket(sockfd)

	server := net.GetHostByName(c.Str("localhost"))
	if server == nil || server.AddrList == nil || *server.AddrList == nil {
		panic("hostname lookup failed")
	}

	servAddr := &net.SockaddrIn{}
	servAddr.Family = net.AF_INET
	servAddr.Port = net.Htons(uint16(1234))
	c.Memcpy(unsafe.Pointer(&servAddr.Addr.Addr), unsafe.Pointer(*server.AddrList), uintptr(server.Length))

	if res := connectSocket(sockfd, (*net.SockAddr)(unsafe.Pointer(servAddr)), unsafe.Sizeof(*servAddr)); res < 0 {
		println("connect error:", socketError())
		panic("connect failed")
	}
	length := c.Strlen(msg)
	if sent := sendSocket(sockfd, unsafe.Pointer(msg), length); sent != int64(length) {
		panic("send failed")
	}
}
