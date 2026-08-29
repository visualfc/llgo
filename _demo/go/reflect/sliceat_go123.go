//go:build go1.23

package main

import (
	"reflect"
	"unsafe"
)

func checkSliceAt() {
	array := [5]int{10, 20, 30, 40, 50}
	value := reflect.SliceAt(reflect.TypeOf(0), unsafe.Pointer(&array[1]), 3)
	slice := value.Interface().([]int)
	if !reflect.DeepEqual(slice, []int{20, 30, 40}) {
		panic("SliceAt")
	}
	slice[0] = 99
	if array[1] != 99 {
		panic("SliceAt alias")
	}
}
