package gotest

import (
	"reflect"
	"testing"
)

type reflectCallLargeReturnPair struct {
	Integer int64
	Float   float64
}

func reflectCallLargeReturn() (int64, float64, complex128, reflectCallLargeReturnPair) {
	return 47, 5.25, complex(4.5, -1.25), reflectCallLargeReturnPair{Integer: 16, Float: 5.5}
}

func TestReflectCallLargeReturnBuffer(t *testing.T) {
	got := reflect.ValueOf(reflectCallLargeReturn).Call(nil)
	if len(got) != 4 || got[0].Int() != 47 || got[1].Float() != 5.25 ||
		got[2].Complex() != complex(4.5, -1.25) ||
		got[3].Interface().(reflectCallLargeReturnPair) != (reflectCallLargeReturnPair{Integer: 16, Float: 5.5}) {
		t.Fatalf("reflect call returned %#v", got)
	}
}
