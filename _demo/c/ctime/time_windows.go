//go:build windows

package main

import "github.com/goplus/lib/c/time"

func main() {
	println("time:", time.Time(nil))
	println("clock:", time.Clock())
}
