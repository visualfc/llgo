package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/cpp/std"
)

func testCPPString() {
	s := std.Str("Hello world\n")
	c.Printf(s.CStr())
	got, size := s.Str(), s.Size()
	print(got, size, "\n")
	if got != "Hello world\n" || size != 12 {
		panic("C++ string round trip failed")
	}
}
