//go:build windows

package main

import "github.com/goplus/lib/c/net"

func socketError() int { return int(net.LastError()) }
