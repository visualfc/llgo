// LITTEST
package main

import (
	"reflect"
	"unsafe"
)

type Plain struct {
	N int
}

// CHECK-LABEL: define void @main.main(){{.*}} {
func main() {
	typ := reflect.TypeOf(Plain{})
	want := reflect.TypeOf((*Plain)(nil))
	newValue := reflect.New
	if newValue(typ).Type() != want {
		panic("New returned a non-canonical pointer type")
	}

	var value Plain
	newValueAt := reflect.NewAt
	if newValueAt(typ, unsafe.Pointer(&value)).Type() != want {
		panic("NewAt returned a non-canonical pointer type")
	}
	addr := reflect.ValueOf(&value).Elem().Addr
	if addr().Type() != want {
		panic("Addr returned a non-canonical pointer type")
	}

	println("ok")
}
