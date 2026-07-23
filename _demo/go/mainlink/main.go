package main

import _ "unsafe"

func main() {
	linkdemo()
}

func demo() {
	println("demo")
}

//go:linkname linkdemo main.demo
func linkdemo()
