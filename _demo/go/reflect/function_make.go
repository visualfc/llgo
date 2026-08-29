package main

import (
	"reflect"
	"strconv"
)

type itoaFunc func(int) string

func testReflectMakeFunc() {
	closureType := reflect.TypeOf((func(func(int) int) func(int) int)(nil))
	closure := reflect.MakeFunc(closureType, func(args []reflect.Value) []reflect.Value {
		input := args[0].Interface().(func(int) int)
		return []reflect.Value{reflect.ValueOf(func(n int) int { return input(n + 100) })}
	})
	result := closure.Call([]reflect.Value{reflect.ValueOf(func(n int) int { return n + 100 })})
	if got := result[0].Call([]reflect.Value{reflect.ValueOf(100)})[0].Int(); got != 300 {
		panic("MakeFunc closure")
	}

	namedType := reflect.TypeOf((*itoaFunc)(nil)).Elem()
	named := reflect.MakeFunc(namedType, func(args []reflect.Value) []reflect.Value {
		return []reflect.Value{reflect.ValueOf(strconv.Itoa(int(args[0].Int())))}
	})
	funcType := reflect.FuncOf([]reflect.Type{namedType}, []reflect.Type{reflect.TypeOf("")}, false)
	caller := reflect.MakeFunc(funcType, func(args []reflect.Value) []reflect.Value {
		return args[0].Call([]reflect.Value{reflect.ValueOf(100)})
	})
	if got := caller.Call([]reflect.Value{named})[0].String(); got != "100" {
		panic("FuncOf named function")
	}

	// Empty aggregates exercise zero-sized ABI slots around non-empty values.
	emptyStruct := reflect.TypeOf(struct{}{})
	emptyArray := reflect.TypeOf([0]int{})
	mixedType := reflect.FuncOf(
		[]reflect.Type{emptyStruct, reflect.TypeOf(0), emptyArray, emptyStruct, reflect.TypeOf("")},
		[]reflect.Type{emptyStruct, reflect.TypeOf(0), emptyArray, reflect.TypeOf("")}, false,
	)
	mixed := reflect.MakeFunc(mixedType, func(args []reflect.Value) []reflect.Value {
		if args[4].String() != "hello world" {
			panic("empty aggregate tail argument")
		}
		return []reflect.Value{args[0], reflect.ValueOf(int(args[1].Int()) + len(args[4].String())), args[2], args[4]}
	})
	mixedResult := mixed.Call([]reflect.Value{
		reflect.ValueOf(struct{}{}), reflect.ValueOf(100), reflect.ValueOf([0]int{}), reflect.ValueOf(struct{}{}), reflect.ValueOf("hello world"),
	})
	if mixedResult[1].Int() != 111 || mixedResult[0].Interface() != struct{}{} || mixedResult[2].Interface() != [0]int{} || mixedResult[3].String() != "hello world" {
		panic("empty aggregate ABI")
	}
}
