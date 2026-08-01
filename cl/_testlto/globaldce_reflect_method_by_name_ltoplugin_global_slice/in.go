// LITTEST
package main

import "reflect"

// SYMBOL-NOT: main{{.*}}S{{.*}}Drop
// SYMBOL-DAG: main{{.*}}S{{.*}}KeepA
// SYMBOL-DAG: main{{.*}}S{{.*}}KeepB
// SYMBOL-NOT: main{{.*}}S{{.*}}Drop

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
