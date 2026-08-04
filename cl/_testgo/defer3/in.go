// LITTEST
package main

// CHECK-DAG: call ptr @"{{.*}}/runtime/internal/runtime.GetThreadDefer"()
// CHECK-DAG: call i32 @{{.*}}setjmp(ptr %{{.*}}{{.*}})
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.Panic"({{.*}})
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.Rethrow"(ptr %{{.*}})

func f(s string) bool {
	return len(s) > 2
}

func fail() {
	defer println("bye")
	panic("panic message")
}

func main() {
	defer func() {
		println("hi")
	}()
	if s := "hello"; f(s) {
		defer println(s)
	} else {
		defer println("world")
		return
	}
	fail()
	println("unreachable")
}
