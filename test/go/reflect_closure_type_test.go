package gotest

import (
	"reflect"
	"testing"
)

func TestReflectClosureParamTypeString(t *testing.T) {
	fn := func(func(int) bool) {}
	typ := reflect.TypeOf(fn)
	if got, want := typ.String(), "func(func(int) bool)"; got != want {
		t.Fatalf("function parameter type = %q, want %q", got, want)
	}
	if got, want := typ.In(0).Kind(), reflect.Func; got != want {
		t.Fatalf("function parameter kind = %v, want %v", got, want)
	}
}

type reflectClosureFieldHolder struct {
	F     func(int) bool
	Named reflectNamedFunc
}

type reflectNamedFunc func(int) bool

func TestReflectClosureNestedTypeString(t *testing.T) {
	field := reflect.TypeOf(reflectClosureFieldHolder{}).Field(0).Type
	if got, want := field.String(), "func(int) bool"; got != want {
		t.Fatalf("struct field type = %q, want %q", got, want)
	}
	if got, want := field.Kind(), reflect.Func; got != want {
		t.Fatalf("struct field kind = %v, want %v", got, want)
	}
	elem := reflect.TypeOf([]func(int) bool{}).Elem()
	if got, want := elem.String(), "func(int) bool"; got != want {
		t.Fatalf("slice element type = %q, want %q", got, want)
	}
	if got, want := elem.Kind(), reflect.Func; got != want {
		t.Fatalf("slice element kind = %v, want %v", got, want)
	}
	mapElem := reflect.TypeOf(map[int]func(int) bool{}).Elem()
	if got, want := mapElem.String(), "func(int) bool"; got != want {
		t.Fatalf("map element type = %q, want %q", got, want)
	}
	if got, want := mapElem.Kind(), reflect.Func; got != want {
		t.Fatalf("map element kind = %v, want %v", got, want)
	}

	captured := 42
	functions := map[int]func() int{0: func() int { return captured }}
	value := reflect.ValueOf(functions).MapIndex(reflect.ValueOf(0))
	if got, want := value.Kind(), reflect.Func; got != want {
		t.Fatalf("map value kind = %v, want %v", got, want)
	}
	if got := value.Interface().(func() int)(); got != captured {
		t.Fatalf("map value returned %d, want %d", got, captured)
	}

	dynamicCaptured := "dynamic"
	dynamicFunction := func(s string) string { return dynamicCaptured + s }
	dynamicType := reflect.MapOf(reflect.TypeOf(0), reflect.TypeOf(dynamicFunction))
	dynamic := reflect.MakeMap(dynamicType)
	dynamic.SetMapIndex(reflect.ValueOf(0), reflect.ValueOf(dynamicFunction))
	dynamicValue := dynamic.MapIndex(reflect.ValueOf(0))
	if got, want := dynamicValue.Kind(), reflect.Func; got != want {
		t.Fatalf("dynamic map value kind = %v, want %v", got, want)
	}
	if got, want := dynamicValue.Interface().(func(string) string)(" map"), "dynamic map"; got != want {
		t.Fatalf("dynamic map value returned %q, want %q", got, want)
	}
}

func TestReflectClosurePointerTypeIdentity(t *testing.T) {
	holder := &reflectClosureFieldHolder{}
	holderType := reflect.TypeOf(*holder)
	holderValue := reflect.ValueOf(holder).Elem()

	plainField := holderType.Field(0).Type
	plainPointer := reflect.PointerTo(plainField)
	plainAddr := holderValue.Field(0).Addr().Type()
	if plainAddr != plainPointer {
		t.Fatalf("function field address type = %v, want %v", plainAddr, plainPointer)
	}

	namedParam := reflect.TypeOf(func(*reflectNamedFunc) {}).In(0)
	namedField := holderType.Field(1).Type
	namedPointer := reflect.PointerTo(namedField)
	namedAddr := holderValue.Field(1).Addr().Type()
	if namedParam != namedPointer || namedAddr != namedPointer {
		t.Fatalf("named function pointer types differ: parameter=%v field=%v address=%v", namedParam, namedPointer, namedAddr)
	}
	types := map[reflect.Type]bool{namedParam: true}
	if !types[namedPointer] || !types[namedAddr] {
		t.Fatalf("canonical named function pointer type was not found as a reflect.Type map key")
	}
}
