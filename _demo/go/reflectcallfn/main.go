package main

import (
	"reflect"
)

type M struct {
	M1 func(int) int
	M2 func(int) int
}

type N struct {
	N1 func(int) int
	N2 func(int) int
	M  M
}

func demo2(fn func(n int) int) (func(int) int, func(int) int) {
	return func(n int) int {
			return fn(n + 100)
		}, func(n int) int {
			return fn(n + 200)
		}
}

func demo1(fn func(n int) int) func(n int) int {
	return func(n int) int {
		return fn(n + 100)
	}
}

func demo3(fn func(n int) int) N {
	return N{
		N1: func(n int) int { return fn(n) + 100 },
		N2: func(n int) int { return fn(n) + 200 },
		M: M{
			M1: func(n int) int { return fn(n) + 300 },
			M2: func(n int) int { return fn(n) + 400 },
		},
	}
}

func main() {
	var base = 100
	fn := func(n int) int {
		return n + base
	}
	// demo1
	f1 := reflect.ValueOf(demo1)
	r1 := f1.Call([]reflect.Value{reflect.ValueOf(fn)})
	if r1[0].Call([]reflect.Value{reflect.ValueOf(100)})[0].Int() != 300 {
		panic("call demo1 error")
	}
	// demo2
	f2 := reflect.ValueOf(demo2)
	r2 := f2.Call([]reflect.Value{reflect.ValueOf(fn)})
	if r2[0].Call([]reflect.Value{reflect.ValueOf(100)})[0].Int() != 300 {
		panic("call demo2 error")
	}
	if r2[1].Call([]reflect.Value{reflect.ValueOf(100)})[0].Int() != 400 {
		panic("call demo2 error")
	}
	// demo3
	f3 := reflect.ValueOf(demo3)
	r3 := f3.Call([]reflect.Value{reflect.ValueOf(fn)})
	if r3[0].Field(0).Call([]reflect.Value{reflect.ValueOf(100)})[0].Int() != 300 {
		panic("call N.N1 error")
	}
	if r3[0].Field(1).Call([]reflect.Value{reflect.ValueOf(100)})[0].Int() != 400 {
		panic("call N.N2 error")
	}
	if r3[0].Field(2).Field(0).Call([]reflect.Value{reflect.ValueOf(100)})[0].Int() != 500 {
		panic("call N.M.M1 error")
	}
	if r3[0].Field(2).Field(1).Call([]reflect.Value{reflect.ValueOf(100)})[0].Int() != 600 {
		panic("call N.M.M2 error")
	}
	if r3[0].Field(2).Field(0).Call([]reflect.Value{reflect.ValueOf(100)})[0].Int() != 500 {
		panic("call N.M.M1 error")
	}
}
