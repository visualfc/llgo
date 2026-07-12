// LITTEST
package main

import "reflect"

// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_loop{{.*}}S{{.*}}Drop
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_loop{{.*}}S{{.*}}KeepLoopA
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_loop{{.*}}S{{.*}}KeepLoopB
// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_loop{{.*}}S{{.*}}Drop

type S struct{}

//go:noinline
func (S) KeepLoopA() string {
	return "loop-a"
}

//go:noinline
func (S) KeepLoopB() string {
	return "loop-b"
}

//go:noinline
func (S) Drop() string {
	panic("Drop should be unreachable")
}

var loopNames = [2]string{"KeepLoopA", "KeepLoopB"}

func main() {
	v := reflect.ValueOf(S{})
	t := reflect.TypeOf(S{})
	for i := range loopNames {
		name := loopNames[i]
		out := v.MethodByName(name).Call(nil)
		println(out[0].String())

		m, ok := t.MethodByName(name)
		if !ok {
			panic("missing method")
		}
		out = m.Func.Call([]reflect.Value{v})
		println(out[0].String())
	}
}
