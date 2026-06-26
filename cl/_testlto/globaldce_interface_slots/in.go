// LITTEST
package main

// CHECK-DAG: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.A:func() int")
// CHECK-DAG: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.B:func(int) int")
// CHECK-DAG: !"go.method.A:func() int"
// CHECK-DAG: !"go.method.B:func(int) int"
// CHECK-DAG: !"go.method.value.reflect"
// CHECK-DAG: !"go.method.type.reflect"

type I interface {
	A() int
	B(int) int
}

type T struct{}

func (T) A() int {
	return 7
}

func (T) B(v int) int {
	return v + 5
}

func main() {
	var i I = T{}
	println(i.A(), i.B(9))
}
