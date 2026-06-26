// LITTEST
package main

import "reflect"

// CHECK-DAG: @"_llgo_{{.*}}globaldce_reflect_type_method.S" = weak_odr constant {{.*}}, !type !{{[0-9]+}}, !type !{{[0-9]+}}, !type !{{[0-9]+}}, !vcall_visibility !{{[0-9]+}}
// CHECK-DAG: @"*_llgo_{{.*}}globaldce_reflect_type_method.S" = weak_odr constant {{.*}}, !type !{{[0-9]+}}, !type !{{[0-9]+}}, !type !{{[0-9]+}}, !vcall_visibility !{{[0-9]+}}
// CHECK-LABEL: define void @"github.com/goplus/llgo/cl/_testlto/globaldce_reflect_type_method.main"
// CHECK: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.Method:func(int) reflect.Method")
// CHECK-NOT: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.value.reflect")
// CHECK: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.type.reflect")
// CHECK-NOT: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.value.reflect")
// CHECK-NOT: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.type.reflect")
// CHECK-DAG: !{i64 {{[0-9]+}}, !"go.method.value.reflect"}
// CHECK-DAG: !{i64 {{[0-9]+}}, !"go.method.type.reflect"}

type S struct{}

func (S) Keep() string {
	return "keep"
}

func main() {
	m := reflect.TypeOf(S{}).Method(0)
	out := m.Func.Call([]reflect.Value{reflect.ValueOf(S{})})
	println(out[0].String())
}
