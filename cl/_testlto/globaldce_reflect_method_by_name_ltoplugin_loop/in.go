// LITTEST
package main

import "reflect"

// SYMBOL-NOT: main{{.*}}S{{.*}}Drop
// SYMBOL-DAG: main{{.*}}S{{.*}}KeepLoopA
// SYMBOL-DAG: main{{.*}}S{{.*}}KeepLoopB
// SYMBOL-NOT: main{{.*}}S{{.*}}Drop

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
	for _, name := range loopNames {
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
