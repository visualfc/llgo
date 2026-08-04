// LITTEST
package main

// CHECK-DAG: call ptr @"{{.*}}/runtime/internal/runtime.GetThreadDefer"()
// CHECK-DAG: call i32 @{{.*}}setjmp(ptr %{{.*}}{{.*}})
// CHECK-DAG: call %"{{.*}}/runtime/internal/runtime.eface" @"{{.*}}/runtime/internal/runtime.Recover"()
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.Panic"({{.*}})

func f(s string) bool {
	return len(s) > 2
}

func fail() {
	defer println("bye")
	defer func() {
		if e := recover(); e != nil {
			println("recover:", e.(string))
		}
	}()
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
	println("reachable")
}
