package main

import (
	"reflect"
	"runtime"
)

// This case owns dynamic composite type construction and representative value
// allocation. Identity checks cover the runtime type caches without copying
// the standard library's Cartesian matrices.
func testDynamicValues() {
	intType := reflect.TypeOf(0)
	stringType := reflect.TypeOf("")
	arrayType := reflect.ArrayOf(3, intType)
	sliceType := reflect.SliceOf(intType)
	mapType := reflect.MapOf(stringType, intType)
	chanType := reflect.ChanOf(reflect.BothDir, intType)
	funcType := reflect.FuncOf([]reflect.Type{intType}, []reflect.Type{stringType}, false)

	if arrayType != reflect.ArrayOf(3, intType) || sliceType != reflect.SliceOf(intType) || mapType != reflect.MapOf(stringType, intType) {
		panic("dynamic type cache")
	}
	if chanType != reflect.TypeOf((chan int)(nil)) || funcType.Kind() != reflect.Func {
		panic("dynamic chan/func type")
	}
	if reflect.PointerTo(chanType).Elem() != chanType {
		panic("ChanOf PointerTo chain")
	}
	slice := reflect.MakeSlice(sliceType, 2, 4)
	slice.Index(0).SetInt(10)
	if slice.Len() != 2 || slice.Cap() != 4 || slice.Index(0).Int() != 10 {
		panic("MakeSlice")
	}
	m := reflect.MakeMapWithSize(mapType, 1)
	m.SetMapIndex(reflect.ValueOf("answer"), reflect.ValueOf(42))
	if m.MapIndex(reflect.ValueOf("answer")).Int() != 42 {
		panic("MakeMapWithSize")
	}
	ch := reflect.MakeChan(chanType, 1)
	ch.Send(reflect.ValueOf(7))
	if value, ok := ch.Recv(); !ok || value.Int() != 7 {
		panic("MakeChan")
	}
	runtime.GC()
	if reflect.PointerTo(sliceType).Elem() != sliceType {
		panic("PointerTo")
	}
}
