//go:build windows && 386

package main

import "unsafe"

func verify() {
	asmFull("nop", nil)

	addr := uintptr(unsafe.Pointer(&testVar))
	asmFull("movl {value}, ({addr})", map[string]any{
		"addr":  addr,
		"value": 43,
	})
	check(43, testVar)

	res1 := asmFull("movl {value}, {}", map[string]any{
		"value": 41,
	})
	check(41, int(res1))

	res2 := asmFull("leal ({a},{b}), {}", map[string]any{
		"a": 25,
		"b": 17,
	})
	check(42, int(res2))
}
