package main

import "reflect"

func demo(fn func(n int) int) func(n int) int {
	return func(n int) int {
		return fn(n + 100)
	}
}

func main() {
	var base = 100
	fn := func(n int) int {
		return n + base
	}
	f := reflect.MakeFunc(reflect.TypeOf(demo), func(args []reflect.Value) []reflect.Value {
		fn := reflect.ValueOf(func(n int) int {
			return args[0].Interface().(func(int) int)(n + 100)
		})
		return []reflect.Value{fn}
	})
	r := f.Call([]reflect.Value{reflect.ValueOf(fn)})
	if r[0].Call([]reflect.Value{reflect.ValueOf(100)})[0].Int() != 300 {
		panic("call fn error")
	}
}
