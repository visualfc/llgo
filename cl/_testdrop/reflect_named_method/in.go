// LITTEST
package main

import "reflect"

// SYMBOL-NOT: testdrop/reflect_named_method{{.*}}T{{.*}}Drop
// SYMBOL-DAG: testdrop/reflect_named_method{{.*}}T{{.*}}Keep
// SYMBOL-NOT: testdrop/reflect_named_method{{.*}}T{{.*}}Drop

type T struct {
	n int
}

//go:noinline
func (t T) Keep() int {
	return t.n + 1
}

//go:noinline
func (t T) Drop() int {
	panic("Drop should be unreachable")
}

func main() {
	v := reflect.ValueOf(T{n: 41})
	out := v.MethodByName("Keep").Call(nil)
	println(out[0].Int())
}
