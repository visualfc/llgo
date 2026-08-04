// LITTEST
package main

// CHECK-DAG: @main.VarName = global %"{{.*}}/runtime/internal/runtime.String" { ptr @{{.*}}, {{(i32|i64)}} 12 }
// CHECK-DAG: @main.VarPlain = global %"{{.*}}/runtime/internal/runtime.String" zeroinitializer
// CHECK-DAG: call void @"{{.*}}/cl/_testgo/rewrite/dep.PrintVar"()

import (
	"fmt"
	"runtime"

	dep "github.com/goplus/llgo/cl/_testgo/rewrite/dep"
)

var VarName = "main-default"
var VarPlain string

func printLine(label, value string) {
	fmt.Printf("%s: %s\n", label, value)
}

func main() {
	printLine("main.VarName", VarName)
	printLine("main.VarPlain", VarPlain)
	dep.PrintVar()
	printLine("runtime.GOROOT()", runtime.GOROOT())
	printLine("runtime.Version()", runtime.Version())
}
