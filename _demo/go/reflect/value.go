package main

import "reflect"

type visibleInner struct {
	Exported int
	hidden   string
}

type visibleOuter struct {
	Name string
	visibleInner
	Value float64
}

type indirectPerson struct {
	Name string
	Age  int
}

func testValueOperations() {
	values := []int{1, 2, 3, 4, 5}
	if n := reflect.Copy(reflect.ValueOf(values[1:]), reflect.ValueOf(values)); n != 4 {
		panic("overlapping Copy count")
	}
	if !reflect.DeepEqual(values, []int{1, 1, 2, 3, 4}) {
		panic("overlapping Copy")
	}
	bytes := make([]byte, 4)
	if n := reflect.Copy(reflect.ValueOf(bytes), reflect.ValueOf("llgo")); n != 4 || string(bytes) != "llgo" {
		panic("string Copy")
	}
	arrayDestination := [3]int{}
	if n := reflect.Copy(reflect.ValueOf(&arrayDestination).Elem(), reflect.ValueOf([]int{7, 8, 9, 10})); n != 3 || arrayDestination != [3]int{7, 8, 9} {
		panic("slice to array Copy")
	}
	arraySource := [2]int{5, 6}
	sliceDestination := []int{0, 0, 0}
	if n := reflect.Copy(reflect.ValueOf(sliceDestination), reflect.ValueOf(arraySource)); n != 2 || !reflect.DeepEqual(sliceDestination, []int{5, 6, 0}) {
		panic("array to slice Copy")
	}
	shouldPanic("call of reflect.Copy on int Value", func() {
		reflect.Copy(reflect.ValueOf(1), reflect.ValueOf([]int{1}))
	})
	shouldPanic("unaddressable value", func() {
		reflect.Copy(reflect.ValueOf([2]int{}), reflect.ValueOf([]int{1, 2}))
	})
	shouldPanic("reflect.Copy:", func() {
		reflect.Copy(reflect.ValueOf([]int{0}), reflect.ValueOf([]int32{1}))
	})

	n := 42
	if reflect.Indirect(reflect.ValueOf(&n)).Int() != 42 || reflect.Indirect(reflect.ValueOf(n)).Int() != 42 {
		panic("Indirect")
	}
	var nilPointer *int
	if reflect.Indirect(reflect.ValueOf(nilPointer)).IsValid() {
		panic("Indirect nil")
	}
	person := indirectPerson{Name: "Alice", Age: 30}
	if got := reflect.Indirect(reflect.ValueOf(person)).Interface().(indirectPerson); got != person {
		panic("Indirect struct value")
	}
	personPointer := &indirectPerson{Name: "Bob", Age: 25}
	if got := reflect.Indirect(reflect.ValueOf(personPointer)).Interface().(indirectPerson); got != *personPointer {
		panic("Indirect struct pointer")
	}

	typ := reflect.TypeOf(visibleOuter{})
	fields := reflect.VisibleFields(typ)
	if len(fields) != 5 || fields[2].Name != "Exported" || !reflect.DeepEqual(fields[2].Index, []int{1, 0}) {
		panic("VisibleFields")
	}
	value := reflect.ValueOf(visibleOuter{visibleInner: visibleInner{Exported: 7}})
	if value.FieldByIndex(fields[2].Index).Int() != 7 {
		panic("FieldByIndex")
	}
	for _, field := range fields {
		fieldValue := value.FieldByIndex(field.Index)
		if fieldValue.CanInterface() != (field.PkgPath == "") {
			panic("VisibleFields CanInterface")
		}
		if fieldValue.CanInterface() {
			_ = fieldValue.Interface()
		}
	}
	checkSliceAt()
}
