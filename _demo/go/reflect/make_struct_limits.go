package main

import (
	"fmt"
	. "reflect"
)

func TestStructOfDifferentPkgPath(t *testingT) {
	fields := []StructField{
		{
			Name:    "f1",
			PkgPath: "p1",
			Type:    TypeOf(int(0)),
		},
		{
			Name:    "f2",
			PkgPath: "p2",
			Type:    TypeOf(int(0)),
		},
	}
	shouldPanic("different PkgPath", func() {
		StructOf(fields)
	})
}

func TestStructOfTooLarge(t *testingT) {
	t1 := TypeOf(byte(0))
	t2 := TypeOf(int16(0))
	t4 := TypeOf(int32(0))
	t0 := ArrayOf(0, t1)

	// 2^64-3 sized type (or 2^32-3 on 32-bit archs)
	bigType := StructOf([]StructField{
		{Name: "F1", Type: ArrayOf(int(^uintptr(0)>>1), t1)},
		{Name: "F2", Type: ArrayOf(int(^uintptr(0)>>1-1), t1)},
	})

	type test struct {
		shouldPanic bool
		fields      []StructField
	}

	tests := [...]test{
		{
			shouldPanic: false, // 2^64-1, ok
			fields: []StructField{
				{Name: "F1", Type: bigType},
				{Name: "F2", Type: ArrayOf(2, t1)},
			},
		},
		{
			shouldPanic: true, // overflow in total size
			fields: []StructField{
				{Name: "F1", Type: bigType},
				{Name: "F2", Type: ArrayOf(3, t1)},
			},
		},
		{
			shouldPanic: true, // overflow while aligning F2
			fields: []StructField{
				{Name: "F1", Type: bigType},
				{Name: "F2", Type: t4},
			},
		},
		{
			shouldPanic: true, // overflow while adding trailing byte for zero-sized fields
			fields: []StructField{
				{Name: "F1", Type: bigType},
				{Name: "F2", Type: ArrayOf(2, t1)},
				{Name: "F3", Type: t0},
			},
		},
		{
			shouldPanic: true, // overflow while aligning total size
			fields: []StructField{
				{Name: "F1", Type: t2},
				{Name: "F2", Type: bigType},
			},
		},
	}

	for i, tt := range tests {
		func() {
			defer func() {
				err := recover()
				if !tt.shouldPanic {
					if err != nil {
						t.Errorf("test %d should not panic, got %s", i, err)
					}
					return
				}
				if err == nil {
					t.Errorf("test %d expected to panic", i)
					return
				}
				s := fmt.Sprintf("%s", err)
				if s != "reflect.StructOf: struct size would exceed virtual address space" {
					t.Errorf("test %d wrong panic message: %s", i, s)
					return
				}
			}()
			_ = StructOf(tt.fields)
		}()
	}
}

type D1 struct {
	d int
}
type D2 struct {
	d int
}

func TestStructOfAnonymous(t *testingT) {
	var s any = struct{ D1 }{}
	f := TypeOf(s).Field(0)
	ds := StructOf([]StructField{f})
	st := TypeOf(s)
	dt := New(ds).Elem()
	if st != dt.Type() {
		t.Errorf("StructOf returned %s, want %s", dt.Type(), st)
	}

	// This should not panic.
	_ = dt.Interface().(struct{ D1 })
}
