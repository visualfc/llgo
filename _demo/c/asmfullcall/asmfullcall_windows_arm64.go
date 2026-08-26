//go:build windows && arm64

package main

import "unsafe"

func verify() {
	asmFull("nop", nil)

	addr := uintptr(unsafe.Pointer(&testVar))
	asmFull("str {value}, [{addr}]", map[string]any{
		"addr":  addr,
		"value": 43,
	})
	check(43, testVar)

	res1 := asmFull("mov {}, {value}", map[string]any{
		"value": 41,
	})
	check(41, int(res1))

	res2 := asmFull("add {}, {a}, {b}", map[string]any{
		"a": 25,
		"b": 17,
	})
	check(42, int(res2))
}
