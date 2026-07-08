package main

import "runtime"

//go:noinline
func inner() {
	panic("manual-boom")
}

//go:noinline
func outer() {
	inner()
}

func main() {
	_ = runtime.NumCPU()
	outer()
}
