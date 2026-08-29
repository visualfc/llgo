package main

import _ "unsafe"

const LLGoFiles = "wrap/wrap.c"

// One mixed integer aggregate exercises register classification and padding.
type registerPair struct {
	tag   int8
	value int64
}

//go:linkname roundtripPair C.roundtrip_pair
func roundtripPair(registerPair) registerPair

// Nine floats are large enough to use the aggregate-result (sret) path.
type largeResult struct{ values [9]float32 }

//go:linkname makeLarge C.make_large
func makeLarge(float32) largeResult

type callbackValue struct{ values [4]int32 }

//llgo:type C
type aggregateCallback func(callbackValue) callbackValue

//go:linkname callCallback C.call_callback
func callCallback(aggregateCallback, callbackValue) callbackValue

//go:linkname callGoExport C.call_go_export
func callGoExport(callbackValue) callbackValue

func goCallback(value callbackValue) callbackValue {
	value.values[1] += 20
	return value
}

//export go_export
func go_export(value callbackValue) callbackValue {
	value.values[2] += 30
	return value
}

func main() {
	if got := roundtripPair(registerPair{tag: 7, value: 40}); got.tag != 8 || got.value != 42 {
		panic("register aggregate ABI")
	}
	large := makeLarge(10)
	if large.values[0] != 10 || large.values[4] != 14 || large.values[8] != 18 {
		panic("C large-result ABI")
	}
	input := callbackValue{values: [4]int32{1, 2, 3, 4}}
	if got := callCallback(goCallback, input); got.values != [4]int32{1, 22, 3, 4} {
		panic("C-to-Go aggregate callback ABI")
	}
	if got := callGoExport(input); got.values != [4]int32{1, 2, 33, 4} {
		panic("C-to-exported-Go aggregate ABI")
	}
	testSRet()
}
