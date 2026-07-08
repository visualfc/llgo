package main

import (
	"reflect"
)

func main() {
	fn := reflect.ValueOf(reflect.New)
	typ := reflect.TypeOf(0)
	v := reflect.ValueOf(typ)
	r := fn.Call([]reflect.Value{v})
	e := r[0].Interface().(reflect.Value).Elem()
	if e.Kind() != reflect.Int {
		panic("error kind")
	}
	e.SetInt(100)
	if e.Interface().(int) != 100 {
		panic("error value")
	}
}
