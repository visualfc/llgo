// LITTEST
package main

import "reflect"

// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_global_slice{{.*}}S{{.*}}Drop
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_global_slice{{.*}}S{{.*}}KeepA
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_global_slice{{.*}}S{{.*}}KeepB
// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_global_slice{{.*}}S{{.*}}Drop

type S struct{}

//go:noinline
func (S) KeepA() string { return "keep-a" }

//go:noinline
func (S) KeepB() string { return "keep-b" }

//go:noinline
func (S) Drop() string { panic("Drop should be unreachable") }

type callbackType string

var callbackTypes = []callbackType{"KeepA", "KeepB"}

func main() {
	v := reflect.ValueOf(S{})
	for _, name := range callbackTypes {
		out := v.MethodByName(string(name)).Call(nil)
		println(out[0].String())
	}
}
