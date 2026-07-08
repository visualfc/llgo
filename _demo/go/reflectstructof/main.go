package main

import (
	"reflect"
)

func add(a int, b int) int {
	return a + b
}

func sub(a int, b int) int {
	return a - b
}

func main() {
	typ := reflect.StructOf([]reflect.StructField{
		{
			Name:    "Add",
			PkgPath: "",
			Type:    reflect.TypeOf(add),
		},
		{
			Name:    "Sub",
			PkgPath: "",
			Type:    reflect.TypeOf(sub),
		},
	})
	st := reflect.New(typ).Elem()
	st.Field(0).Set(reflect.ValueOf(add))
	st.Field(1).Set(reflect.ValueOf(sub))
	r := st.Field(0).Call([]reflect.Value{reflect.ValueOf(1), reflect.ValueOf(2)})
	if len(r) != 1 || r[0].Interface() != 3 {
		panic("st.Add(1,2) error")
	}
	r = st.Field(1).Call([]reflect.Value{reflect.ValueOf(1), reflect.ValueOf(2)})
	if len(r) != 1 || r[0].Interface() != -1 {
		panic("st.Sub(1,2) error")
	}
}
