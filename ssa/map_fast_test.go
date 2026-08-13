package ssa

import (
	"go/token"
	"go/types"
	"testing"
)

func TestMapKeyFastKind(t *testing.T) {
	namedUint32 := types.NewNamed(
		types.NewTypeName(token.NoPos, nil, "ID", nil),
		types.Typ[types.Uint32],
		nil,
	)
	namedString := types.NewNamed(
		types.NewTypeName(token.NoPos, nil, "Name", nil),
		types.Typ[types.String],
		nil,
	)
	structKey := types.NewStruct(
		[]*types.Var{types.NewField(token.NoPos, nil, "value", types.Typ[types.Uint64], false)},
		nil,
	)
	ptrKey := types.NewPointer(types.Typ[types.Int])
	chanKey := types.NewChan(types.SendRecv, types.Typ[types.Int])

	tests := []struct {
		name    string
		key     types.Type
		ptrSize int
		want    mapFastKind
	}{
		{"uint32", types.Typ[types.Uint32], 8, mapFast32},
		{"named uint32", namedUint32, 8, mapFast32},
		{"int32", types.Typ[types.Int32], 8, mapFast32},
		{"uint64", types.Typ[types.Uint64], 8, mapFast64},
		{"int64", types.Typ[types.Int64], 8, mapFast64},
		{"int on 32-bit", types.Typ[types.Int], 4, mapFast32},
		{"int on 64-bit", types.Typ[types.Int], 8, mapFast64},
		{"uintptr on 32-bit", types.Typ[types.Uintptr], 4, mapFast32},
		{"uintptr on 64-bit", types.Typ[types.Uintptr], 8, mapFast64},
		{"string", types.Typ[types.String], 8, mapFastStr},
		{"named string", namedString, 8, mapFastStr},
		{"unsafe pointer on 32-bit", types.Typ[types.UnsafePointer], 4, mapFast32Ptr},
		{"unsafe pointer on 64-bit", types.Typ[types.UnsafePointer], 8, mapFast64Ptr},
		{"pointer on 32-bit", ptrKey, 4, mapFast32Ptr},
		{"pointer on 64-bit", ptrKey, 8, mapFast64Ptr},
		{"channel on 64-bit", chanKey, 8, mapFast64Ptr},
		{"float32 fallback", types.Typ[types.Float32], 8, mapFastNone},
		{"float64 fallback", types.Typ[types.Float64], 8, mapFastNone},
		{"struct fallback", structKey, 8, mapFastNone},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			mapType := types.NewMap(test.key, types.Typ[types.Int])
			if got := mapKeyFastKind(mapType, test.ptrSize); got != test.want {
				t.Fatalf("mapKeyFastKind(map[%v]int, %d) = %v, want %v", test.key, test.ptrSize, got, test.want)
			}
		})
	}
}

func TestMapFastRuntimeNames(t *testing.T) {
	tests := []struct {
		kind       mapFastKind
		access1    string
		access2    string
		assign     string
		deleteName string
	}{
		{mapFastNone, "MapAccess1", "MapAccess2", "MapAssign", "MapDelete"},
		{mapFast32, "MapAccess1Fast32", "MapAccess2Fast32", "MapAssignFast32", "MapDeleteFast32"},
		{mapFast64, "MapAccess1Fast64", "MapAccess2Fast64", "MapAssignFast64", "MapDeleteFast64"},
		{mapFast32Ptr, "MapAccess1Fast32", "MapAccess2Fast32", "MapAssignFast32Ptr", "MapDeleteFast32"},
		{mapFast64Ptr, "MapAccess1Fast64", "MapAccess2Fast64", "MapAssignFast64Ptr", "MapDeleteFast64"},
		{mapFastStr, "MapAccess1FastStr", "MapAccess2FastStr", "MapAssignFastStr", "MapDeleteFastStr"},
	}

	for _, test := range tests {
		if got := test.kind.accessName(false); got != test.access1 {
			t.Errorf("%v access1 = %q, want %q", test.kind, got, test.access1)
		}
		if got := test.kind.accessName(true); got != test.access2 {
			t.Errorf("%v access2 = %q, want %q", test.kind, got, test.access2)
		}
		if got := test.kind.assignName(); got != test.assign {
			t.Errorf("%v assign = %q, want %q", test.kind, got, test.assign)
		}
		if got := test.kind.deleteName(); got != test.deleteName {
			t.Errorf("%v delete = %q, want %q", test.kind, got, test.deleteName)
		}
	}
}
