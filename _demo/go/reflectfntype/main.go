package main

import (
	"reflect"
)

type Func func(int) int

type T struct {
	X  int
	Y  int
	Fn Func
}

func demo(n int, f Func) Func {
	return f
}

func main() {
	t := reflect.TypeOf((*T)(nil)).Elem()
	if t.Field(2).Type.Kind() != reflect.Func {
		panic("filed type error")
	}
	f := reflect.ValueOf(demo).Type()
	if f.In(1).Kind() != reflect.Func {
		panic("in type error")
	}
	if f.Out(0).Kind() != reflect.Func {
		panic("in type error")
	}
}
