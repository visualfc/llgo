package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
)

func main() {
	a := [...]int{100, 8, 23, 2, 7}
	c.Qsort(c.Pointer(&a), 5, unsafe.Sizeof(0), func(a, b c.Pointer) c.Int {
		return c.Int(*(*int)(a) - *(*int)(b))
	})
	want := [...]int{2, 7, 8, 23, 100}
	for i, v := range a {
		if v != want[i] {
			panic("qsort result")
		}
	}
}
