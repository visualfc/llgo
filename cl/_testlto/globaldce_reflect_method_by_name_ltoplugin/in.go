// LITTEST
package main

import (
	"os"
	"reflect"
)

// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin{{.*}}S{{.*}}Drop
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin{{.*}}S{{.*}}KeepA
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin{{.*}}S{{.*}}KeepB
// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin{{.*}}S{{.*}}Drop

type S struct{}

//go:noinline
func (S) KeepA() string {
	return "keep-a"
}

//go:noinline
func (S) KeepB() string {
	return "keep-b"
}

//go:noinline
func (S) Drop() string {
	panic("Drop should be unreachable")
}

func methodName() string {
	name := "KeepA"
	if os.Args[0] == "" {
		name = "KeepB"
	}
	return name
}

func main() {
	out := reflect.ValueOf(S{}).MethodByName(methodName()).Call(nil)
	println(out[0].String())

	m, ok := reflect.TypeOf(S{}).MethodByName(methodName())
	if !ok {
		panic("missing method")
	}
	out = m.Func.Call([]reflect.Value{reflect.ValueOf(S{})})
	println(out[0].String())
}
