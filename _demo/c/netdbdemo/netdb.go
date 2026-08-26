package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/net"
)

func main() {
	var hints net.AddrInfo
	hints.Family = net.AF_UNSPEC
	hints.SockType = net.SOCK_STREAM

	host := "httpbin.org"
	port := "80"

	var result *net.AddrInfo
	if resultCode := net.Getaddrinfo(c.Str(host), c.Str(port), &hints, &result); resultCode != 0 {
		panic("getaddrinfo failed")
	}
	if result == nil {
		panic("getaddrinfo returned no addresses")
	}
	net.Freeaddrinfo(result)
	c.Printf(c.Str("resolved %s:%s\n"), c.Str(host), c.Str(port))
	c.Fflush(c.Stdout)
}
