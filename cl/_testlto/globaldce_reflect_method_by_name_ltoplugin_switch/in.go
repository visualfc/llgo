// LITTEST
package main

import (
	"os"
	"reflect"
)

// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_switch{{.*}}S{{.*}}Drop
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_switch{{.*}}S{{.*}}KeepA
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_switch{{.*}}S{{.*}}KeepB
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_switch{{.*}}S{{.*}}KeepC
// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_switch{{.*}}S{{.*}}Drop

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
func (S) KeepC() string {
	return "keep-c"
}

//go:noinline
func (S) Drop() string {
	panic("Drop should be unreachable")
}

//go:noinline
func selector() int {
	return len(os.Args)
}

func methodName() string {
	switch selector() {
	case 1:
		return "KeepA"
	case 2:
		return "KeepB"
	default:
		return "KeepC"
	}
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
