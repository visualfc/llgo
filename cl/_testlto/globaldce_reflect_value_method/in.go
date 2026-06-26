// LITTEST
package main

import "reflect"

// CHECK-DAG: @"_llgo_{{.*}}globaldce_reflect_value_method.S" = weak_odr constant {{.*}}, !type !{{[0-9]+}}, !type !{{[0-9]+}}, !type !{{[0-9]+}}, !vcall_visibility !{{[0-9]+}}
// CHECK-DAG: @"*_llgo_{{.*}}globaldce_reflect_value_method.S" = weak_odr constant {{.*}}, !type !{{[0-9]+}}, !type !{{[0-9]+}}, !type !{{[0-9]+}}, !vcall_visibility !{{[0-9]+}}
// CHECK-LABEL: define void @"github.com/goplus/llgo/cl/_testlto/globaldce_reflect_value_method.main"
// CHECK-NOT: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.type.reflect")
// CHECK: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.value.reflect")
// CHECK-NOT: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.type.reflect")
// CHECK-NOT: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.value.reflect")
// CHECK-DAG: !{i64 {{[0-9]+}}, !"go.method.value.reflect"}
// CHECK-DAG: !{i64 {{[0-9]+}}, !"go.method.type.reflect"}

type S struct{}

func (S) Keep() string {
	return "keep"
}

func main() {
	out := reflect.ValueOf(S{}).MethodByName("Keep").Call(nil)
	println(out[0].String())
}
