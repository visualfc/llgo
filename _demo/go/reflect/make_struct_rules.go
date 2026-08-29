package main

import (
	"go/token"
	. "reflect"
	"runtime"
)

func TestStructOfExportRules(t *testingT) {
	type S1 struct{}
	type s2 struct{}
	type ΦType struct{}
	type φType struct{}

	testPanic := func(i int, mustPanic bool, f func()) {
		defer func() {
			err := recover()
			if err == nil && mustPanic {
				t.Errorf("test-%d did not panic", i)
			}
			if err != nil && !mustPanic {
				t.Errorf("test-%d panicked: %v\n", i, err)
			}
		}()
		f()
	}

	tests := []struct {
		field     StructField
		mustPanic bool
		exported  bool
	}{
		{
			field:    StructField{Name: "S1", Anonymous: true, Type: TypeOf(S1{})},
			exported: true,
		},
		{
			field:    StructField{Name: "S1", Anonymous: true, Type: TypeOf((*S1)(nil))},
			exported: true,
		},
		{
			field:     StructField{Name: "s2", Anonymous: true, Type: TypeOf(s2{})},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "s2", Anonymous: true, Type: TypeOf((*s2)(nil))},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "Name", Type: nil, PkgPath: ""},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "", Type: TypeOf(S1{}), PkgPath: ""},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "S1", Anonymous: true, Type: TypeOf(S1{}), PkgPath: "other/pkg"},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "S1", Anonymous: true, Type: TypeOf((*S1)(nil)), PkgPath: "other/pkg"},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "s2", Anonymous: true, Type: TypeOf(s2{}), PkgPath: "other/pkg"},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "s2", Anonymous: true, Type: TypeOf((*s2)(nil)), PkgPath: "other/pkg"},
			mustPanic: true,
		},
		{
			field: StructField{Name: "s2", Type: TypeOf(int(0)), PkgPath: "other/pkg"},
		},
		{
			field: StructField{Name: "s2", Type: TypeOf(int(0)), PkgPath: "other/pkg"},
		},
		{
			field:    StructField{Name: "S", Type: TypeOf(S1{})},
			exported: true,
		},
		{
			field:    StructField{Name: "S", Type: TypeOf((*S1)(nil))},
			exported: true,
		},
		{
			field:    StructField{Name: "S", Type: TypeOf(s2{})},
			exported: true,
		},
		{
			field:    StructField{Name: "S", Type: TypeOf((*s2)(nil))},
			exported: true,
		},
		{
			field:     StructField{Name: "s", Type: TypeOf(S1{})},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "s", Type: TypeOf((*S1)(nil))},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "s", Type: TypeOf(s2{})},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "s", Type: TypeOf((*s2)(nil))},
			mustPanic: true,
		},
		{
			field: StructField{Name: "s", Type: TypeOf(S1{}), PkgPath: "other/pkg"},
		},
		{
			field: StructField{Name: "s", Type: TypeOf((*S1)(nil)), PkgPath: "other/pkg"},
		},
		{
			field: StructField{Name: "s", Type: TypeOf(s2{}), PkgPath: "other/pkg"},
		},
		{
			field: StructField{Name: "s", Type: TypeOf((*s2)(nil)), PkgPath: "other/pkg"},
		},
		{
			field:     StructField{Name: "", Type: TypeOf(ΦType{})},
			mustPanic: true,
		},
		{
			field:     StructField{Name: "", Type: TypeOf(φType{})},
			mustPanic: true,
		},
		{
			field:    StructField{Name: "Φ", Type: TypeOf(0)},
			exported: true,
		},
		{
			field:    StructField{Name: "φ", Type: TypeOf(0)},
			exported: false,
		},
	}

	for i, test := range tests {
		testPanic(i, test.mustPanic, func() {
			typ := StructOf([]StructField{test.field})
			if typ == nil {
				t.Errorf("test-%d: error creating struct type", i)
				return
			}
			field := typ.Field(0)
			n := field.Name
			if n == "" {
				panic("field.Name must not be empty")
			}
			exported := token.IsExported(n)
			if exported != test.exported {
				t.Errorf("test-%d: got exported=%v want exported=%v", i, exported, test.exported)
			}
			if field.PkgPath != test.field.PkgPath {
				t.Errorf("test-%d: got PkgPath=%q want pkgPath=%q", i, field.PkgPath, test.field.PkgPath)
			}
		})
	}
}

func TestStructOfGC(t *testingT) {
	type T *uintptr
	tt := TypeOf(T(nil))
	fields := []StructField{
		{Name: "X", Type: tt},
		{Name: "Y", Type: tt},
	}
	st := StructOf(fields)

	const n = 10000
	var x []any
	for i := 0; i < n; i++ {
		v := New(st).Elem()
		for j := 0; j < v.NumField(); j++ {
			p := new(uintptr)
			*p = uintptr(i*n + j)
			v.Field(j).Set(ValueOf(p).Convert(tt))
		}
		x = append(x, v.Interface())
	}
	runtime.GC()

	for i, xi := range x {
		v := ValueOf(xi)
		for j := 0; j < v.NumField(); j++ {
			k := v.Field(j).Elem().Interface()
			if k != uintptr(i*n+j) {
				t.Errorf("lost x[%d].%c = %d, want %d", i, "XY"[j], k, i*n+j)
			}
		}
	}
}

func TestStructOfAlg(t *testingT) {
	st := StructOf([]StructField{{Name: "X", Tag: "x", Type: TypeOf(int(0))}})
	v1 := New(st).Elem()
	v2 := New(st).Elem()
	if !DeepEqual(v1.Interface(), v1.Interface()) {
		t.Errorf("constructed struct %v not equal to itself", v1.Interface())
	}
	v1.FieldByName("X").Set(ValueOf(int(1)))
	if i1, i2 := v1.Interface(), v2.Interface(); DeepEqual(i1, i2) {
		t.Errorf("constructed structs %v and %v should not be equal", i1, i2)
	}

	st = StructOf([]StructField{{Name: "X", Tag: "x", Type: TypeOf([]int(nil))}})
	v1 = New(st).Elem()
	shouldPanic("", func() { _ = v1.Interface() == v1.Interface() })
}

func TestStructOfGenericAlg(t *testingT) {
	st1 := StructOf([]StructField{
		{Name: "X", Tag: "x", Type: TypeOf(int64(0))},
		{Name: "Y", Type: TypeOf(string(""))},
	})
	st := StructOf([]StructField{
		{Name: "S0", Type: st1},
		{Name: "S1", Type: st1},
	})

	tests := []struct {
		rt  Type
		idx []int
	}{
		{
			rt:  st,
			idx: []int{0, 1},
		},
		{
			rt:  st1,
			idx: []int{1},
		},
		{
			rt: StructOf(
				[]StructField{
					{Name: "XX", Type: TypeOf([0]int{})},
					{Name: "YY", Type: TypeOf("")},
				},
			),
			idx: []int{1},
		},
		{
			rt: StructOf(
				[]StructField{
					{Name: "XX", Type: TypeOf([0]int{})},
					{Name: "YY", Type: TypeOf("")},
					{Name: "ZZ", Type: TypeOf([2]int{})},
				},
			),
			idx: []int{1},
		},
		{
			rt: StructOf(
				[]StructField{
					{Name: "XX", Type: TypeOf([1]int{})},
					{Name: "YY", Type: TypeOf("")},
				},
			),
			idx: []int{1},
		},
		{
			rt: StructOf(
				[]StructField{
					{Name: "XX", Type: TypeOf([1]int{})},
					{Name: "YY", Type: TypeOf("")},
					{Name: "ZZ", Type: TypeOf([1]int{})},
				},
			),
			idx: []int{1},
		},
		{
			rt: StructOf(
				[]StructField{
					{Name: "XX", Type: TypeOf([2]int{})},
					{Name: "YY", Type: TypeOf("")},
					{Name: "ZZ", Type: TypeOf([2]int{})},
				},
			),
			idx: []int{1},
		},
		{
			rt: StructOf(
				[]StructField{
					{Name: "XX", Type: TypeOf(int64(0))},
					{Name: "YY", Type: TypeOf(byte(0))},
					{Name: "ZZ", Type: TypeOf("")},
				},
			),
			idx: []int{2},
		},
		{
			rt: StructOf(
				[]StructField{
					{Name: "XX", Type: TypeOf(int64(0))},
					{Name: "YY", Type: TypeOf(int64(0))},
					{Name: "ZZ", Type: TypeOf("")},
					{Name: "AA", Type: TypeOf([1]int64{})},
				},
			),
			idx: []int{2},
		},
	}

	for _, table := range tests {
		v1 := New(table.rt).Elem()
		v2 := New(table.rt).Elem()

		if !DeepEqual(v1.Interface(), v1.Interface()) {
			t.Errorf("constructed struct %v not equal to itself", v1.Interface())
		}

		v1.FieldByIndex(table.idx).Set(ValueOf("abc"))
		v2.FieldByIndex(table.idx).Set(ValueOf("def"))
		if i1, i2 := v1.Interface(), v2.Interface(); DeepEqual(i1, i2) {
			t.Errorf("constructed structs %v and %v should not be equal", i1, i2)
		}

		abc := "abc"
		v1.FieldByIndex(table.idx).Set(ValueOf(abc))
		val := "+" + abc + "-"
		v2.FieldByIndex(table.idx).Set(ValueOf(val[1:4]))
		if i1, i2 := v1.Interface(), v2.Interface(); !DeepEqual(i1, i2) {
			t.Errorf("constructed structs %v and %v should be equal", i1, i2)
		}

		// Test hash
		m := MakeMap(MapOf(table.rt, TypeOf(int(0))))
		m.SetMapIndex(v1, ValueOf(1))
		if i1, i2 := v1.Interface(), v2.Interface(); !m.MapIndex(v2).IsValid() {
			t.Errorf("constructed structs %#v and %#v have different hashes", i1, i2)
		}

		v2.FieldByIndex(table.idx).Set(ValueOf("abc"))
		if i1, i2 := v1.Interface(), v2.Interface(); !DeepEqual(i1, i2) {
			t.Errorf("constructed structs %v and %v should be equal", i1, i2)
		}

		if i1, i2 := v1.Interface(), v2.Interface(); !m.MapIndex(v2).IsValid() {
			t.Errorf("constructed structs %v and %v have different hashes", i1, i2)
		}
	}
}

func TestStructOfDirectIface(t *testingT) {
	{
		type T struct{ X [1]*byte }
		i1 := Zero(TypeOf(T{})).Interface()
		v1 := ValueOf(&i1).Elem()
		p1 := v1.InterfaceData()[1]

		i2 := Zero(StructOf([]StructField{
			{
				Name: "X",
				Type: ArrayOf(1, TypeOf((*int8)(nil))),
			},
		})).Interface()
		v2 := ValueOf(&i2).Elem()
		p2 := v2.InterfaceData()[1]

		if p1 != 0 {
			t.Errorf("got p1=%v. want=%v", p1, nil)
		}

		if p2 != 0 {
			t.Errorf("got p2=%v. want=%v", p2, nil)
		}
	}
	{
		type T struct{ X [0]*byte }
		i1 := Zero(TypeOf(T{})).Interface()
		v1 := ValueOf(&i1).Elem()
		p1 := v1.InterfaceData()[1]

		i2 := Zero(StructOf([]StructField{
			{
				Name: "X",
				Type: ArrayOf(0, TypeOf((*int8)(nil))),
			},
		})).Interface()
		v2 := ValueOf(&i2).Elem()
		p2 := v2.InterfaceData()[1]

		if p1 == 0 {
			t.Errorf("got p1=%v. want=not-%v", p1, nil)
		}

		if p2 == 0 {
			t.Errorf("got p2=%v. want=not-%v", p2, nil)
		}
	}
}
