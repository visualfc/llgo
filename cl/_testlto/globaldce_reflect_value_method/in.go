// LITTEST
package main

import "reflect"

// CHECK: metadata !"go.method.value.reflect"
// CHECK-NOT: metadata !"go.method.type.reflect"
// CHECK-DAG: !"go.method.value.reflect"
// CHECK-DAG: !"go.method.type.reflect"

type S struct{}

func (S) Keep() string {
	return "keep"
}

func main() {
	out := reflect.ValueOf(S{}).MethodByName("Keep").Call(nil)
	println(out[0].String())
}
