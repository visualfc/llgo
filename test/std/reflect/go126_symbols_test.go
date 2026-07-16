//go:build go1.26

package reflect_test

import (
	"reflect"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var value reflect.Value
	_ = reflect.TypeAssert[any]
	_ = value.Fields
	_ = value.Methods
}
