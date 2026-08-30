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
