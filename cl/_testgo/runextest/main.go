// LITTEST
package main

// CHECK-DAG: define {{(i32|i64)}} @main.Zoo(){{.*}} {
// CHECK-DAG: call {{(i32|i64)}} @"{{.*}}/cl/_testgo/runextest/foo.Foo"()
// CHECK-DAG: call {{(i32|i64)}} @"{{.*}}/cl/_testgo/runextest/bar.Bar"()

import (
	"github.com/goplus/llgo/cl/_testgo/runextest/bar"
	"github.com/goplus/llgo/cl/_testgo/runextest/foo"
)

func Zoo() int {
	return 3
}

func main() {
	println("foo.Foo()", foo.Foo())
	println("bar.Bar()", bar.Bar())
	println("Zoo()", Zoo())
}
