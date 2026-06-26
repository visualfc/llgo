// LITTEST
package main

// CHECK-DAG: !"Virtual Function Elim"
// CHECK-DAG: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.A:func() int")
// CHECK-DAG: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.B:func(int) int")
// CHECK-DAG: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.C:func() string")
// CHECK-DAG: @"_llgo_{{.*}}/cl/_testlto/globaldce_interface_matrix.T1" = weak_odr constant {{.*}}, !type ![[T1A:[0-9]+]], !type !{{[0-9]+}}, !type !{{[0-9]+}}, !type ![[T1B:[0-9]+]], !type !{{[0-9]+}}, !type !{{[0-9]+}}, !type ![[T1C:[0-9]+]], !type !{{[0-9]+}}, !type !{{[0-9]+}}, !vcall_visibility
// CHECK-DAG: @"*_llgo_{{.*}}/cl/_testlto/globaldce_interface_matrix.T2" = weak_odr constant {{.*}}, !type ![[T2A:[0-9]+]], !type !{{[0-9]+}}, !type !{{[0-9]+}}, !type ![[T2B:[0-9]+]], !type !{{[0-9]+}}, !type !{{[0-9]+}}, !type ![[T2C:[0-9]+]], !type !{{[0-9]+}}, !type !{{[0-9]+}}, !vcall_visibility
// CHECK-DAG: ![[T1A]] = !{i64 {{[0-9]+}}, !"go.method.A:func() int"}
// CHECK-DAG: ![[T1B]] = !{i64 {{[0-9]+}}, !"go.method.B:func(int) int"}
// CHECK-DAG: ![[T1C]] = !{i64 {{[0-9]+}}, !"go.method.C:func() string"}
// CHECK-DAG: ![[T2A]] = !{i64 {{[0-9]+}}, !"go.method.A:func() int"}
// CHECK-DAG: ![[T2B]] = !{i64 {{[0-9]+}}, !"go.method.B:func(int) int"}
// CHECK-DAG: ![[T2C]] = !{i64 {{[0-9]+}}, !"go.method.C:func() string"}
// CHECK-DAG: !"go.method.value.reflect"
// CHECK-DAG: !"go.method.type.reflect"

type Base interface {
	A() int
}

type Embedded interface {
	Base
	B(int) int
}

type I interface {
	Embedded
	C() string
}

type T1 struct{}

func (T1) A() int {
	return 11
}

func (T1) B(v int) int {
	return v + 21
}

func (T1) C() string {
	return "one"
}

type T2 struct {
	n int
}

func (t *T2) A() int {
	return t.n
}

func (t *T2) B(v int) int {
	return v + t.n
}

func (*T2) C() string {
	return "two"
}

func use(i I) {
	println(i.A(), i.B(3), i.C())
}

func useBase(b Base) {
	println(b.A())
}

func main() {
	use(T1{})
	use(&T2{n: 12})
	useBase(&T2{n: 15})
}
