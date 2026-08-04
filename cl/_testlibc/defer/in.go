// LITTEST
package main

// CHECK-DAG: call ptr @"{{.*}}/runtime/internal/runtime.GetThreadDefer"()
// CHECK-DAG: call i32 @{{.*}}setjmp(ptr %{{.*}}{{.*}})
// CHECK-DAG: call i32 (ptr, ...) @printf({{.*}})

import "github.com/goplus/lib/c"

func f(s string) bool {
	return len(s) > 2
}

func main() {
	c.GoDeferData()
	if s := "hello"; f(s) {
		defer c.Printf(c.Str("%s\n"), c.AllocaCStr(s))
	} else {
		defer c.Printf(c.Str("world\n"))
	}
	defer c.Printf(c.Str("bye\n"))
}
