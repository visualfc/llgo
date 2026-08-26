package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/net"
)

func main() {
	sockfd := net.Socket(net.AF_INET, net.SOCK_STREAM, 0)
	if sockfd == net.InvalidSocket {
		panic("socket failed")
	}
	msg := c.Str("Hello, World!")
	defer net.Close(sockfd)

	server := net.GetHostByName(c.Str("localhost"))
	if server == nil || server.AddrList == nil || *server.AddrList == nil {
		panic("hostname lookup failed")
	}

	servAddr := &net.SockaddrIn{}
	servAddr.Family = net.AF_INET
	servAddr.Port = net.Htons(uint16(1234))
	c.Memcpy(unsafe.Pointer(&servAddr.Addr.Addr), unsafe.Pointer(*server.AddrList), uintptr(server.Length))

	if res := net.Connect(sockfd, (*net.SockAddr)(unsafe.Pointer(servAddr)), net.SocklenT(unsafe.Sizeof(*servAddr))); res < 0 {
		println("connect error:", socketError())
		panic("connect failed")
	}
	length := c.Strlen(msg)
	if sent := net.Send(sockfd, unsafe.Pointer(msg), length, 0); sent != c.Long(length) {
		panic("send failed")
	}
}
