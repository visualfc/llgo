// LITTEST
package main

// CHECK-DAG: define void @main.main(){{.*}} {
// CHECK-DAG: call ptr @"{{.*}}/runtime/internal/runtime.GetThreadDefer"()
// CHECK-DAG: call i32 @{{.*}}setjmp(ptr %{{.*}}{{.*}})
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.Rethrow"(ptr %{{.*}})

func main() {
	syms := []int{}
	for range syms {
	}
	defer println("bye")
	for range syms {
	}
}
