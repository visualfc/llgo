package main

import (
	"math"
	"reflect"
	"runtime"
)

type convertedInt int64
type convertedString string
type convertedBytes []byte
type convertedFunc func(int, int) int

func testConversions() {
	checks := []struct {
		in   any
		out  reflect.Type
		want any
	}{
		{int32(42), reflect.TypeOf(int64(0)), int64(42)},
		{float64(3.75), reflect.TypeOf(int(0)), int(3)},
		{convertedInt(9), reflect.TypeOf(int64(0)), int64(9)},
		{convertedString("llgo"), reflect.TypeOf([]byte{}), []byte("llgo")},
		{[]rune("Go"), reflect.TypeOf(convertedString("")), convertedString("Go")},
		{convertedBytes("ffi"), reflect.TypeOf(string("")), "ffi"},
	}
	for _, check := range checks {
		value := reflect.ValueOf(check.in)
		if !value.CanConvert(check.out) || !reflect.DeepEqual(value.Convert(check.out).Interface(), check.want) {
			panic("representative conversion")
		}
	}

	slice := []byte{1, 2, 3, 4}
	arrayType := reflect.TypeOf([4]byte{})
	array := reflect.ValueOf(slice).Convert(arrayType).Interface().([4]byte)
	slice[0] = 9
	if array != [4]byte{1, 2, 3, 4} {
		panic("slice-to-array copy")
	}
	short := reflect.ValueOf(slice[:2])
	longPointer := reflect.TypeOf((*[4]byte)(nil))
	if short.CanConvert(longPointer) {
		panic("short slice converted to array pointer")
	}

	offset := 100
	fn := func(a, b int) int { return a + b + offset }
	converted := reflect.ValueOf(fn).Convert(reflect.TypeOf(convertedFunc(nil)))
	if got := converted.Call([]reflect.Value{reflect.ValueOf(1), reflect.ValueOf(2)})[0].Int(); got != 103 {
		panic("function conversion")
	}

	// Store/load and reflect conversion must preserve a signaling NaN payload.
	const signalingNaN uint32 = 0x7f800001
	type namedFloat float32
	value := namedFloat(math.Float32frombits(signalingNaN))
	runtime.Gosched()
	plain := reflect.ValueOf(value).Convert(reflect.TypeOf(float32(0))).Interface().(float32)
	if math.Float32bits(plain) != signalingNaN {
		panic("NaN conversion")
	}
}
