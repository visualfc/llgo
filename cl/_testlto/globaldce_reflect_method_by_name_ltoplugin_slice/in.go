// LITTEST
package main

import "reflect"

// SYMBOL-NOT: main{{.*}}S{{.*}}Drop
// SYMBOL-DAG: main{{.*}}S{{.*}}KeepSliceValue
// SYMBOL-DAG: main{{.*}}S{{.*}}KeepSliceType
// SYMBOL-NOT: main{{.*}}S{{.*}}Drop

type S struct{}

//go:noinline
func (S) KeepSliceValue() string {
	return "slice-value"
}

//go:noinline
func (S) KeepSliceType() string {
	return "slice-type"
}

//go:noinline
func (S) Drop() string {
	panic("Drop should be unreachable")
}

func valueName() string {
	return "__KeepSliceValue__"[2:16]
}

func typeName() string {
	return "__KeepSliceType__"[2:15]
}

func main() {
	v := reflect.ValueOf(S{})
	out := v.MethodByName(valueName()).Call(nil)
	println(out[0].String())

	m, ok := reflect.TypeOf(S{}).MethodByName(typeName())
	if !ok {
		panic("missing method")
	}
	out = m.Func.Call([]reflect.Value{v})
	println(out[0].String())
}
