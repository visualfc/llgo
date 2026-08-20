//go:build windows

package main

func main() {
	var recovered any
	func() {
		defer func() {
			recovered = recover()
		}()
		var deferred func()
		defer deferred()
		panic(1)
	}()
	if recovered == nil || recovered == 1 {
		panic("nil deferred call did not replace the original panic")
	}
}
