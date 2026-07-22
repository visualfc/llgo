// LITTEST
package main

import "reflect"

// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_range_literal{{.*}}S{{.*}}Drop
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_range_literal{{.*}}S{{.*}}Query
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_range_literal{{.*}}S{{.*}}Mutation
// SYMBOL-DAG: globaldce_reflect_method_by_name_ltoplugin_range_literal{{.*}}S{{.*}}Subscription
// SYMBOL-NOT: globaldce_reflect_method_by_name_ltoplugin_range_literal{{.*}}S{{.*}}Drop

const (
	Query        = "Query"
	Mutation     = "Mutation"
	Subscription = "Subscription"
)

type S struct{}

//go:noinline
func (S) Query() string {
	return "query"
}

//go:noinline
func (S) Mutation() string {
	return "mutation"
}

//go:noinline
func (S) Subscription() string {
	return "subscription"
}

//go:noinline
func (S) Drop() string {
	panic("Drop should be unreachable")
}

func main() {
	rv := reflect.ValueOf(S{})
	for _, op := range [...]string{Query, Mutation, Subscription} {
		m := rv.MethodByName(op)
		if !m.IsValid() {
			panic("missing method")
		}
		out := m.Call(nil)
		println(out[0].String())
	}
}
