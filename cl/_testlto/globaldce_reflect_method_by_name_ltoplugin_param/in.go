// LITTEST
package main

import "reflect"

// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_param{{.*}}S{{.*}}Drop
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_param{{.*}}S{{.*}}KeepParamValueA
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_param{{.*}}S{{.*}}KeepParamValueB
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_param{{.*}}S{{.*}}KeepParamTypeA
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_param{{.*}}S{{.*}}KeepParamTypeB
// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_param{{.*}}S{{.*}}Drop

type S struct{}

//go:noinline
func (S) KeepParamValueA() string {
	return "param-value-a"
}

//go:noinline
func (S) KeepParamValueB() string {
	return "param-value-b"
}

//go:noinline
func (S) KeepParamTypeA() string {
	return "param-type-a"
}

//go:noinline
func (S) KeepParamTypeB() string {
	return "param-type-b"
}

//go:noinline
func (S) Drop() string {
	panic("Drop should be unreachable")
}

//go:noinline
func callValueByName(name string) string {
	out := reflect.ValueOf(S{}).MethodByName(name).Call(nil)
	return out[0].String()
}

//go:noinline
func forwardValueSuffix(suffix string) string {
	return callValueByName("Keep" + suffix)
}

//go:noinline
func callTypeByName(name string) string {
	m, ok := reflect.TypeOf(S{}).MethodByName(name)
	if !ok {
		panic("missing method")
	}
	out := m.Func.Call([]reflect.Value{reflect.ValueOf(S{})})
	return out[0].String()
}

//go:noinline
func forwardTypeName(name string) string {
	return callTypeByName(name)
}

func main() {
	println(forwardValueSuffix("__ParamValueA__"[2:13]))
	println(forwardValueSuffix("__ParamValueB__"[2:13]))
	println(forwardTypeName("KeepParamTypeA"))
	println(forwardTypeName("KeepParamTypeB"))
}
