package gotest

import (
	"reflect"
	"testing"
)

type reflectCallNamer interface {
	Name() string
}

type reflectCallName string

func (n reflectCallName) Name() string { return string(n) }

func reflectCallInterfaceLess(x, y reflectCallNamer) bool {
	return x.Name() < y.Name()
}

func reflectCallInterfaceIsNil(x reflectCallNamer) bool { return x == nil }

func TestReflectCallInterfaceSliceElements(t *testing.T) {
	values := []reflectCallNamer{reflectCallName("b"), reflectCallName("a")}
	slice := reflect.ValueOf(values)
	out := reflect.ValueOf(reflectCallInterfaceLess).Call([]reflect.Value{
		slice.Index(0),
		slice.Index(1),
	})
	if got := out[0].Bool(); got {
		t.Fatalf("reflected interface call result = %v, want false", got)
	}

	nilValues := []reflectCallNamer{nil}
	nilOut := reflect.ValueOf(reflectCallInterfaceIsNil).Call([]reflect.Value{
		reflect.ValueOf(nilValues).Index(0),
	})
	if got := nilOut[0].Bool(); !got {
		t.Fatalf("reflected nil interface call result = %v, want true", got)
	}
}
