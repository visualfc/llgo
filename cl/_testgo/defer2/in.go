// LITTEST
package main

// CHECK-DAG: call ptr @"{{.*}}/runtime/internal/runtime.GetThreadDefer"()
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.SetThreadDefer"(ptr %{{.*}})
// CHECK-DAG: call i32 @{{.*}}setjmp(ptr %{{.*}}{{.*}})
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.Rethrow"(ptr %{{.*}})

func f(s string) bool {
	return len(s) > 2
}

func main() {
	if s := "hello"; f(s) {
		defer println(s)
	} else {
		defer println("world")
		return
	}
	defer println("bye")
}
