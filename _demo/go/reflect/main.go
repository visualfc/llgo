package main

import (
	"fmt"
	"log"
	"reflect"
	"strings"
)

type testingT struct{}

func (t *testingT) Errorf(format string, args ...any) { log.Panicf(format, args...) }
func (t *testingT) Fatal(args ...any)                 { log.Panic(args...) }
func (t *testingT) Fatalf(format string, args ...any) { log.Panicf(format, args...) }

func main() {
	var t testingT

	// Keep the complete dynamic-type matrix that the original demo executed.
	TestArrayOf(&t)
	TestArrayOfAlg(&t)
	TestArrayOfGenericAlg(&t)
	TestArrayOfDirectIface(&t)
	TestArrayOfPanicOnNegativeLength(&t)
	TestSliceOf(&t)
	TestSliceOfGC(&t)
	TestStructOf(&t)
	TestStructOfGC(&t)
	TestStructOfAlg(&t)
	TestStructOfGenericAlg(&t)
	TestStructOfDirectIface(&t)
	TestStructOfExportRules(&t)
	TestStructOfFieldName(&t)
	TestStructOfAnonymous(&t)
	TestStructOfTooLarge(&t)
	TestStructOfDifferentPkgPath(&t)
	TestChanOfDir(&t)
	TestMapOf(&t)
	TestFuncOf(&t)

	// Keep the complete receiver/return ABI matrix that the original demo ran.
	TestMethod(&t)
	TestMethodValue(&t)
	TestVariadicMethodValue(&t)
	TestDirectIfaceMethod(&t)
	TestMethod5(&t)
	TestMethodSmall(&t)
	TestMethodFloat(&t)

	testReflectPointerMetadata()
	testReflectStructOf()
	testDynamicValues()
	testMethodExtras()
	testReflectCall()
	testReflectMakeFunc()
	testFunctionMetadata()
	testConversions()
	TestConvertFunc(&t)
	testValueOperations()
}

func shouldPanic(expect string, f func()) {
	defer func() {
		r := recover()
		if r == nil {
			panic("did not panic")
		}
		if expect == "" {
			return
		}
		var s string
		switch r := r.(type) {
		case string:
			s = r
		case *reflect.ValueError:
			s = r.Error()
		default:
			panic(fmt.Sprintf("panicked with unexpected type %T", r))
		}
		if !strings.HasPrefix(s, "reflect") {
			panic(`panic string does not start with "reflect": ` + s)
		}
		if !strings.Contains(s, expect) {
			panic(`panic string does not contain "` + expect + `": ` + s)
		}
	}()
	f()
}
