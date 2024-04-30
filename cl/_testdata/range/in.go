package main

import _ "unsafe"

//go:linkname printf C.printf
func printf(format *int8, __llgo_va_list ...any)

var hello = [...]int8{'h', 'e', 'l', 'l', 'o', '\n', 0}

func main() {
	var n int
	for k := range [...]int{1, 2, 3, 4} {
		n += k
	}
	if n == 6 {
		printf(&hello[0])
	}
}
