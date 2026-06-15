// LITTEST
package main

import "reflect"

// CHECK-DAG: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.reflect")
// CHECK-DAG: !"go.method.Keep:func() string"
// CHECK-DAG: !"go.method.hidden:func() string"
// CHECK-DAG: !"go.method.reflect"

type S struct{}

func (S) Keep() string {
	return "keep"
}

func (S) hidden() string {
	return "hidden"
}

func main() {
	out := reflect.ValueOf(S{}).MethodByName("Keep").Call(nil)
	println(out[0].String())
}
