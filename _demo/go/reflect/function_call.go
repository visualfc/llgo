package main

import "reflect"

type callbacks struct {
	First  func(int) int
	Second func(int) int
}

type nestedCallbacks struct {
	First  func(int) int
	Second func(int) int
	More   callbacks
}

func makeCallbacks(fn func(int) int) (func(int) int, callbacks) {
	return func(n int) int { return fn(n + 100) }, callbacks{
		First:  func(n int) int { return fn(n) + 200 },
		Second: func(n int) int { return fn(n) + 300 },
	}
}

func makeSingleCallback(fn func(int) int) func(int) int {
	return func(n int) int { return fn(n + 100) }
}

func makeTwoCallbacks(fn func(int) int) (func(int) int, func(int) int) {
	return func(n int) int { return fn(n + 100) }, func(n int) int { return fn(n + 200) }
}

func makeNestedCallbacks(fn func(int) int) nestedCallbacks {
	return nestedCallbacks{
		First:  func(n int) int { return fn(n + 100) },
		Second: func(n int) int { return fn(n + 200) },
		More: callbacks{
			First:  func(n int) int { return fn(n + 300) },
			Second: func(n int) int { return fn(n + 400) },
		},
	}
}

func testReflectCall() {
	base := 100
	fn := func(n int) int { return n + base }
	single := reflect.ValueOf(makeSingleCallback).Call([]reflect.Value{reflect.ValueOf(fn)})[0]
	if got := single.Call([]reflect.Value{reflect.ValueOf(100)})[0].Int(); got != 300 {
		panic("single closure result")
	}
	two := reflect.ValueOf(makeTwoCallbacks).Call([]reflect.Value{reflect.ValueOf(fn)})
	if got := two[0].Call([]reflect.Value{reflect.ValueOf(100)})[0].Int(); got != 300 {
		panic("first direct closure result")
	}
	if got := two[1].Call([]reflect.Value{reflect.ValueOf(100)})[0].Int(); got != 400 {
		panic("second direct closure result")
	}
	nested := reflect.ValueOf(makeNestedCallbacks).Call([]reflect.Value{reflect.ValueOf(fn)})[0]
	for _, test := range []struct {
		value reflect.Value
		want  int64
	}{
		{nested.FieldByName("First"), 300},
		{nested.FieldByName("Second"), 400},
		{nested.FieldByName("More").FieldByName("First"), 500},
		{nested.FieldByName("More").FieldByName("Second"), 600},
	} {
		if got := test.value.Call([]reflect.Value{reflect.ValueOf(100)})[0].Int(); got != test.want {
			panic("nested closure result")
		}
	}

	result := reflect.ValueOf(makeCallbacks).Call([]reflect.Value{reflect.ValueOf(fn)})
	if got := result[0].Call([]reflect.Value{reflect.ValueOf(100)})[0].Int(); got != 300 {
		panic("closure result")
	}
	aggregate := result[1]
	if got := aggregate.FieldByName("First").Call([]reflect.Value{reflect.ValueOf(100)})[0].Int(); got != 400 {
		panic("aggregate function result")
	}
	if got := aggregate.FieldByName("Second").Call([]reflect.Value{reflect.ValueOf(100)})[0].Int(); got != 500 {
		panic("second aggregate result")
	}

	// Preserve the interface-wrapped reflect.Value return path.
	newResult := reflect.ValueOf(reflect.New).Call([]reflect.Value{reflect.ValueOf(reflect.TypeOf(0))})
	value := newResult[0].Interface().(reflect.Value).Elem()
	value.SetInt(42)
	if value.Interface().(int) != 42 {
		panic("interface wrapped reflect.Value")
	}
}
