package main

import (
	"fmt"
	. "reflect"
	"runtime"
)

func TestStructOfFieldName(t *testingT) {
	// invalid field name "1nvalid"
	shouldPanic("has invalid name", func() {
		StructOf([]StructField{
			{Name: "Valid", Type: TypeOf("")},
			{Name: "1nvalid", Type: TypeOf("")},
		})
	})

	// invalid field name "+"
	shouldPanic("has invalid name", func() {
		StructOf([]StructField{
			{Name: "Val1d", Type: TypeOf("")},
			{Name: "+", Type: TypeOf("")},
		})
	})

	// no field name
	shouldPanic("has no name", func() {
		StructOf([]StructField{
			{Name: "", Type: TypeOf("")},
		})
	})

	// verify creation of a struct with valid struct fields
	validFields := []StructField{
		{
			Name: "φ",
			Type: TypeOf(""),
		},
		{
			Name: "ValidName",
			Type: TypeOf(""),
		},
		{
			Name: "Val1dNam5",
			Type: TypeOf(""),
		},
	}

	validStruct := StructOf(validFields)

	const structStr = `struct { φ string; ValidName string; Val1dNam5 string }`
	if got, want := validStruct.String(), structStr; got != want {
		t.Errorf("StructOf(validFields).String()=%q, want %q", got, want)
	}
}

func TestStructOf(t *testingT) {
	// check construction and use of type not in binary
	fields := []StructField{
		{
			Name: "S",
			Tag:  "s",
			Type: TypeOf(""),
		},
		{
			Name: "X",
			Tag:  "x",
			Type: TypeOf(byte(0)),
		},
		{
			Name: "Y",
			Type: TypeOf(uint64(0)),
		},
		{
			Name: "Z",
			Type: TypeOf([3]uint16{}),
		},
	}

	st := StructOf(fields)
	v := New(st).Elem()
	runtime.GC()
	v.FieldByName("X").Set(ValueOf(byte(2)))
	v.FieldByIndex([]int{1}).Set(ValueOf(byte(1)))
	runtime.GC()

	s := fmt.Sprint(v.Interface())
	want := `{ 1 0 [0 0 0]}`
	if s != want {
		t.Errorf("constructed struct = %s, want %s", s, want)
	}
	const stStr = `struct { S string "s"; X uint8 "x"; Y uint64; Z [3]uint16 }`
	if got, want := st.String(), stStr; got != want {
		t.Errorf("StructOf(fields).String()=%q, want %q", got, want)
	}

	// check the size, alignment and field offsets
	stt := TypeOf(struct {
		String string
		X      byte
		Y      uint64
		Z      [3]uint16
	}{})
	if st.Size() != stt.Size() {
		t.Errorf("constructed struct size = %v, want %v", st.Size(), stt.Size())
	}
	if st.Align() != stt.Align() {
		t.Errorf("constructed struct align = %v, want %v", st.Align(), stt.Align())
	}
	if st.FieldAlign() != stt.FieldAlign() {
		t.Errorf("constructed struct field align = %v, want %v", st.FieldAlign(), stt.FieldAlign())
	}
	for i := 0; i < st.NumField(); i++ {
		o1 := st.Field(i).Offset
		o2 := stt.Field(i).Offset
		if o1 != o2 {
			t.Errorf("constructed struct field %v offset = %v, want %v", i, o1, o2)
		}
	}

	// Check size and alignment with a trailing zero-sized field.
	st = StructOf([]StructField{
		{
			Name: "F1",
			Type: TypeOf(byte(0)),
		},
		{
			Name: "F2",
			Type: TypeOf([0]*byte{}),
		},
	})
	stt = TypeOf(struct {
		G1 byte
		G2 [0]*byte
	}{})
	if st.Size() != stt.Size() {
		t.Errorf("constructed zero-padded struct size = %v, want %v", st.Size(), stt.Size())
	}
	if st.Align() != stt.Align() {
		t.Errorf("constructed zero-padded struct align = %v, want %v", st.Align(), stt.Align())
	}
	if st.FieldAlign() != stt.FieldAlign() {
		t.Errorf("constructed zero-padded struct field align = %v, want %v", st.FieldAlign(), stt.FieldAlign())
	}
	for i := 0; i < st.NumField(); i++ {
		o1 := st.Field(i).Offset
		o2 := stt.Field(i).Offset
		if o1 != o2 {
			t.Errorf("constructed zero-padded struct field %v offset = %v, want %v", i, o1, o2)
		}
	}

	// check duplicate names
	shouldPanic("duplicate field", func() {
		StructOf([]StructField{
			{Name: "string", PkgPath: "p", Type: TypeOf("")},
			{Name: "string", PkgPath: "p", Type: TypeOf("")},
		})
	})
	shouldPanic("has no name", func() {
		StructOf([]StructField{
			{Type: TypeOf("")},
			{Name: "string", PkgPath: "p", Type: TypeOf("")},
		})
	})
	shouldPanic("has no name", func() {
		StructOf([]StructField{
			{Type: TypeOf("")},
			{Type: TypeOf("")},
		})
	})

	// check that type already in binary is found
	checkSameType(t, StructOf(fields[2:3]), struct{ Y uint64 }{})

	// gccgo used to fail this test.
	type structFieldType any
	checkSameType(t,
		StructOf([]StructField{
			{
				Name: "F",
				Type: TypeOf((*structFieldType)(nil)).Elem(),
			},
		}),
		struct{ F structFieldType }{})
}
