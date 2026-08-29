package main

import (
	"fmt"
	. "reflect"
	"runtime"
	"strconv"
)

func checkSameType(t *testingT, x Type, y any) {
	if x != TypeOf(y) || TypeOf(Zero(x).Interface()) != TypeOf(y) {
		t.Errorf("did not find preexisting type for %s (vs %s)", TypeOf(x), TypeOf(y))
	}
}

func TestArrayOf(t *testingT) {
	// check construction and use of type not in binary
	tests := []struct {
		n          int
		value      func(i int) any
		comparable bool
		want       string
	}{
		{
			n:          0,
			value:      func(i int) any { type Tint int; return Tint(i) },
			comparable: true,
			want:       "[]",
		},
		{
			n:          10,
			value:      func(i int) any { type Tint int; return Tint(i) },
			comparable: true,
			want:       "[0 1 2 3 4 5 6 7 8 9]",
		},
		{
			n:          10,
			value:      func(i int) any { type Tfloat float64; return Tfloat(i) },
			comparable: true,
			want:       "[0 1 2 3 4 5 6 7 8 9]",
		},
		{
			n:          10,
			value:      func(i int) any { type Tstring string; return Tstring(strconv.Itoa(i)) },
			comparable: true,
			want:       "[0 1 2 3 4 5 6 7 8 9]",
		},
		{
			n:          10,
			value:      func(i int) any { type Tstruct struct{ V int }; return Tstruct{i} },
			comparable: true,
			want:       "[{0} {1} {2} {3} {4} {5} {6} {7} {8} {9}]",
		},
		{
			n:          10,
			value:      func(i int) any { type Tint int; return []Tint{Tint(i)} },
			comparable: false,
			want:       "[[0] [1] [2] [3] [4] [5] [6] [7] [8] [9]]",
		},
		{
			n:          10,
			value:      func(i int) any { type Tint int; return [1]Tint{Tint(i)} },
			comparable: true,
			want:       "[[0] [1] [2] [3] [4] [5] [6] [7] [8] [9]]",
		},
		{
			n: 10,
			value: func(i int) any {
				type Tstruct struct{ V [1]int }
				return Tstruct{[1]int{i}}
			},
			comparable: true,
			want:       "[{[0]} {[1]} {[2]} {[3]} {[4]} {[5]} {[6]} {[7]} {[8]} {[9]}]",
		},
		{
			n:          10,
			value:      func(i int) any { type Tstruct struct{ V []int }; return Tstruct{[]int{i}} },
			comparable: false,
			want:       "[{[0]} {[1]} {[2]} {[3]} {[4]} {[5]} {[6]} {[7]} {[8]} {[9]}]",
		},
		{
			n:          10,
			value:      func(i int) any { type TstructUV struct{ U, V int }; return TstructUV{i, i} },
			comparable: true,
			want:       "[{0 0} {1 1} {2 2} {3 3} {4 4} {5 5} {6 6} {7 7} {8 8} {9 9}]",
		},
		{
			n: 10,
			value: func(i int) any {
				type TstructUV struct {
					U int
					V float64
				}
				return TstructUV{i, float64(i)}
			},
			comparable: true,
			want:       "[{0 0} {1 1} {2 2} {3 3} {4 4} {5 5} {6 6} {7 7} {8 8} {9 9}]",
		},
	}

	for _, table := range tests {
		at := ArrayOf(table.n, TypeOf(table.value(0)))
		v := New(at).Elem()
		vok := New(at).Elem()
		vnot := New(at).Elem()
		for i := 0; i < v.Len(); i++ {
			v.Index(i).Set(ValueOf(table.value(i)))
			vok.Index(i).Set(ValueOf(table.value(i)))
			j := i
			if i+1 == v.Len() {
				j = i + 1
			}
			vnot.Index(i).Set(ValueOf(table.value(j))) // make it differ only by last element
		}
		s := fmt.Sprint(v.Interface())
		if s != table.want {
			t.Errorf("constructed array = %s, want %s", s, table.want)
		}

		if table.comparable != at.Comparable() {
			t.Errorf("constructed array (%#v) is comparable=%v, want=%v", v.Interface(), at.Comparable(), table.comparable)
		}
		if table.comparable {
			if table.n > 0 {
				if DeepEqual(vnot.Interface(), v.Interface()) {
					t.Errorf(
						"arrays (%#v) compare ok (but should not)",
						v.Interface(),
					)
				}
			}
			if !DeepEqual(vok.Interface(), v.Interface()) {
				t.Errorf(
					"arrays (%#v) compare NOT-ok (but should)",
					v.Interface(),
				)
			}
		}
	}

	// check that type already in binary is found
	type T int
	checkSameType(t, ArrayOf(5, TypeOf(T(1))), [5]T{})
}

func TestArrayOfAlg(t *testingT) {
	at := ArrayOf(6, TypeOf(byte(0)))
	v1 := New(at).Elem()
	v2 := New(at).Elem()
	if v1.Interface() != v1.Interface() {
		t.Errorf("constructed array %v not equal to itself", v1.Interface())
	}
	v1.Index(5).Set(ValueOf(byte(1)))
	if i1, i2 := v1.Interface(), v2.Interface(); i1 == i2 {
		t.Errorf("constructed arrays %v and %v should not be equal", i1, i2)
	}

	at = ArrayOf(6, TypeOf([]int(nil)))
	v1 = New(at).Elem()
	shouldPanic("", func() { _ = v1.Interface() == v1.Interface() })
}

func TestArrayOfGenericAlg(t *testingT) {
	at1 := ArrayOf(5, TypeOf(string("")))
	at := ArrayOf(6, at1)
	v1 := New(at).Elem()
	v2 := New(at).Elem()
	if v1.Interface() != v1.Interface() {
		t.Errorf("constructed array %v not equal to itself", v1.Interface())
	}

	v1.Index(0).Index(0).Set(ValueOf("abc"))
	v2.Index(0).Index(0).Set(ValueOf("efg"))
	if i1, i2 := v1.Interface(), v2.Interface(); i1 == i2 {
		t.Errorf("constructed arrays %v and %v should not be equal", i1, i2)
	}

	v1.Index(0).Index(0).Set(ValueOf("abc"))
	v2.Index(0).Index(0).Set(ValueOf((v1.Index(0).Index(0).String() + " ")[:3]))
	if i1, i2 := v1.Interface(), v2.Interface(); i1 != i2 {
		t.Errorf("constructed arrays %v and %v should be equal", i1, i2)
	}

	// Test hash
	m := MakeMap(MapOf(at, TypeOf(int(0))))
	m.SetMapIndex(v1, ValueOf(1))
	if i1, i2 := v1.Interface(), v2.Interface(); !m.MapIndex(v2).IsValid() {
		t.Errorf("constructed arrays %v and %v have different hashes", i1, i2)
	}
}

func TestArrayOfDirectIface(t *testingT) {
	{
		type T [1]*byte
		i1 := Zero(TypeOf(T{})).Interface()
		v1 := ValueOf(&i1).Elem()
		p1 := v1.InterfaceData()[1]

		i2 := Zero(ArrayOf(1, PointerTo(TypeOf(int8(0))))).Interface()
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
		type T [0]*byte
		i1 := Zero(TypeOf(T{})).Interface()
		v1 := ValueOf(&i1).Elem()
		p1 := v1.InterfaceData()[1]

		i2 := Zero(ArrayOf(0, PointerTo(TypeOf(int8(0))))).Interface()
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

// Ensure passing in negative lengths panics.
// See https://golang.org/issue/43603
func TestArrayOfPanicOnNegativeLength(t *testingT) {
	shouldPanic("reflect: negative length passed to ArrayOf", func() {
		ArrayOf(-1, TypeOf(byte(0)))
	})
}

func TestSliceOf(t *testingT) {
	// check construction and use of type not in binary
	type T int
	st := SliceOf(TypeOf(T(1)))
	if got, want := st.String(), "[]main.T"; got != want {
		t.Errorf("SliceOf(T(1)).String()=%q, want %q", got, want)
	}
	v := MakeSlice(st, 10, 10)
	runtime.GC()
	for i := 0; i < v.Len(); i++ {
		v.Index(i).Set(ValueOf(T(i)))
		runtime.GC()
	}
	s := fmt.Sprint(v.Interface())
	want := "[0 1 2 3 4 5 6 7 8 9]"
	if s != want {
		t.Errorf("constructed slice = %s, want %s", s, want)
	}

	// check that type already in binary is found
	type T1 int
	checkSameType(t, SliceOf(TypeOf(T1(1))), []T1{})
}
