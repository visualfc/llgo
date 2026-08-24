// LITTEST
package main

import (
	"reflect"

	"github.com/xgo-dev/llgo/cl/_testrt/ptrtothislazy/dep"
)

type Local struct {
	N int
}

// CHECK-LABEL: define void @main.main(){{.*}} {
func main() {
	pointerTo := reflect.PointerTo

	// No static *Local type is named by the program, so this exercises the
	// runtime synthesis path rather than typelist canonicalization.
	local := reflect.TypeOf(Local{})
	localPtr := pointerTo(local)
	if localPtr.Kind() != reflect.Pointer || localPtr.Elem() != local {
		panic("failed to synthesize methodless pointer type")
	}

	// Plain comes from an imported package while *Plain is independently
	// referenced. The lookup path must still return the canonical static type.
	plain := reflect.TypeOf(dep.Plain{})
	plainPtr := reflect.TypeOf((*dep.Plain)(nil))
	if pointerTo(plain) != plainPtr {
		panic("PointerTo returned a non-canonical pointer type")
	}
	if reflect.New(plain).Type() != plainPtr {
		panic("New returned a non-canonical pointer type")
	}
	var value dep.Plain
	if reflect.ValueOf(&value).Elem().Addr().Type() != plainPtr {
		panic("Addr returned a non-canonical pointer type")
	}

	// A pointer descriptor with methods must remain static: synthesized pointer
	// descriptors do not contain uncommon method metadata.
	withMethod := reflect.TypeOf(dep.WithMethod{})
	withMethodPtr := reflect.TypeOf((*dep.WithMethod)(nil))
	if pointerTo(withMethod) != withMethodPtr {
		panic("method-bearing pointer type is not canonical")
	}
	if _, ok := withMethodPtr.MethodByName("Value"); !ok {
		panic("promoted value-receiver method is missing")
	}
	if _, ok := withMethodPtr.MethodByName("Pointer"); !ok {
		panic("pointer-receiver method is missing")
	}
	result := reflect.New(withMethod).MethodByName("Pointer").Call(nil)
	if len(result) != 1 || result[0].Int() != 42 {
		panic("pointer-receiver method call failed")
	}

	println("ok")
}
