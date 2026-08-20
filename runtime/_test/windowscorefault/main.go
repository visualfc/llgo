//go:build windows

package main

import (
	// Packages below the LLGo runtime root do not trigger the ordinary runtime
	// injection, so initialize its per-thread defer state explicitly.
	_ "github.com/xgo-dev/llgo/runtime/internal/runtime"
)

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
