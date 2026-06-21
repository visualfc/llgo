// LITTEST
package main

import "reflect"

// CHECK: metadata !"go.method.type.reflect"
// CHECK-NOT: metadata !"go.method.value.reflect"
// CHECK-DAG: !"go.method.value.reflect"
// CHECK-DAG: !"go.method.type.reflect"

type S struct{}

func (S) Keep() string {
	return "keep"
}

func main() {
	m := reflect.TypeOf(S{}).Method(0)
	out := m.Func.Call([]reflect.Value{reflect.ValueOf(S{})})
	println(out[0].String())
}
