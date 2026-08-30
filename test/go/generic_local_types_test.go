package gotest

import (
	"reflect"
	"strings"
	"testing"
)

func genericLocalRuntimeType[T any]() reflect.Type {
	type local struct {
		value T
	}
	return reflect.TypeOf(local{})
}

func TestGenericLocalRuntimeTypesIncludeOuterArgs(t *testing.T) {
	intType := genericLocalRuntimeType[int]()
	stringType := genericLocalRuntimeType[string]()
	sameIntType := genericLocalRuntimeType[int]()
	if intType != sameIntType {
		t.Fatalf("same local generic runtime type has different identities: %v != %v", intType, sameIntType)
	}
	if intType == stringType {
		t.Fatalf("local generic runtime types are identical: %v", intType)
	}
	intName := intType.String()
	stringName := stringType.String()
	if !strings.Contains(intName, "int") {
		t.Fatalf("local generic int type name does not include type argument: %q", intName)
	}
	if !strings.Contains(stringName, "string") {
		t.Fatalf("local generic string type name does not include type argument: %q", stringName)
	}
}

type genericLocalIntish interface{ ~int }

func genericNestedLocalRuntimeTypes[A genericLocalIntish]() (reflect.Type, reflect.Type) {
	type Int int
	type T[B genericLocalIntish] struct{}
	type U[_ any] int
	return reflect.TypeOf(T[int]{}), reflect.TypeOf(T[U[int]]{})
}

func TestGenericNestedLocalRuntimeTypeNames(t *testing.T) {
	direct, nested := genericNestedLocalRuntimeTypes[int]()
	if got, want := direct.String(), "gotest.T[int;int]"; got != want {
		t.Fatalf("direct local generic type name = %q, want %q", got, want)
	}
	nestedName := nested.String()
	const nestedPrefix = "gotest.T[int;github.com/xgo-dev/llgo/test/go.U[int;int]·"
	if !strings.HasPrefix(nestedName, nestedPrefix) || !strings.HasSuffix(nestedName, "]") {
		t.Fatalf("nested local generic type name = %q, want %q plus numeric suffix", nestedName, nestedPrefix)
	}
	ordinal := strings.TrimSuffix(strings.TrimPrefix(nestedName, nestedPrefix), "]")
	if ordinal == "" {
		t.Fatalf("nested local generic type name = %q, want numeric local type suffix", nestedName)
	}
	for _, r := range ordinal {
		if r < '0' || r > '9' {
			t.Fatalf("nested local generic type name = %q, want numeric local type suffix", nestedName)
		}
	}
}
