package reflect_test

import (
	"math/big"
	"reflect"
	"testing"
)

// Test Type.Name and Type.String
func TestTypeName(t *testing.T) {
	type MyInt int
	typ := reflect.TypeOf(MyInt(0))

	name := typ.Name()
	if name != "MyInt" {
		t.Errorf("Type.Name should be 'MyInt', got %q", name)
	}

	str := typ.String()
	if str == "" {
		t.Error("Type.String should not be empty")
	}
}

// Test Type.Kind
func TestTypeKind(t *testing.T) {
	tests := []struct {
		value any
		kind  reflect.Kind
	}{
		{42, reflect.Int},
		{"hello", reflect.String},
		{true, reflect.Bool},
		{3.14, reflect.Float64},
		{[]int{}, reflect.Slice},
		{[3]int{}, reflect.Array},
		{map[string]int{}, reflect.Map},
		{struct{}{}, reflect.Struct},
	}

	for _, tt := range tests {
		typ := reflect.TypeOf(tt.value)
		if typ.Kind() != tt.kind {
			t.Errorf("TypeOf(%v).Kind() = %v, want %v", tt.value, typ.Kind(), tt.kind)
		}
	}
}

func TestTypeForMatchesStructFieldType(t *testing.T) {
	type holder struct {
		Value *big.Int
	}

	direct := reflect.TypeFor[*big.Int]()
	repeated := reflect.TypeFor[*big.Int]()
	field := reflect.TypeFor[holder]().Field(0).Type
	if direct != repeated {
		t.Fatalf("repeated TypeFor returned distinct types: %v and %v", direct, repeated)
	}
	if direct != field {
		t.Fatalf("TypeFor type %v differs from struct field type %v", direct, field)
	}
	value := reflect.ValueOf(&holder{}).Elem().Field(0).Addr().Interface()
	if _, ok := value.(**big.Int); !ok {
		t.Fatalf("reflected field address has type %T, want **big.Int", value)
	}
}
