//go:build windows

package main

import "github.com/goplus/lib/c/setjmp"

func main() {
	var jb setjmp.JmpBuf
	switch ret := setjmp.Setjmp(&jb); ret {
	case 0:
		println("Hello, setjmp!")
		setjmp.Longjmp(&jb, 1)
	default:
		println("exception:", ret)
	}
}
