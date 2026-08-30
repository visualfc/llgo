package gotest

import (
	"reflect"
	"strings"
	"testing"
	"unsafe"
)

type reflectMetadataFoo struct {
	bar int
}

var blankFieldEvalCount int

func blankFieldValue() int {
	blankFieldEvalCount++
	return 2
}

func reflectMakeFuncBoolLoop(next func() bool) bool {
	for b := next(); b; b = next() {
		return true
	}
	return false
}

func recoveredErrorString(v any) string {
	if s, ok := v.(interface{ Error() string }); ok {
		return s.Error()
	}
	return reflect.ValueOf(v).String()
}

func callInvalidReflectMakeFunc() (recovered any) {
	defer func() {
		recovered = recover()
	}()
	fn := reflect.MakeFunc(reflect.TypeOf(func() error { return nil }), func([]reflect.Value) []reflect.Value {
		var out [1]reflect.Value
		return out[:]
	}).Interface().(func() error)
	_ = fn()
	return nil
}

func TestReflectTypeMetadataMakeFunc(t *testing.T) {
	const packagePath = "github.com/xgo-dev/llgo/test/go"
	if got := reflect.ValueOf(reflectMetadataFoo{}).Type().Field(0).PkgPath; got != packagePath {
		t.Fatalf("field PkgPath = %q, want %q", got, packagePath)
	}
	if got := reflect.TypeOf(unsafe.Pointer(nil)).PkgPath(); got != "unsafe" {
		t.Fatalf("unsafe.Pointer PkgPath = %q, want unsafe", got)
	}

	before := blankFieldEvalCount
	x := struct{ a, _, c int }{1, blankFieldValue(), 3}
	if got := blankFieldEvalCount - before; got != 1 {
		t.Fatalf("blank field initializer evaluated %d times, want 1", got)
	}
	if got := reflect.ValueOf(x).Field(1).Int(); got != 0 {
		t.Fatalf("blank field reflect value = %d, want 0", got)
	}

	recovered := callInvalidReflectMakeFunc()
	if recovered == nil {
		t.Fatal("MakeFunc call did not panic")
	}
	if got := recoveredErrorString(recovered); !strings.HasPrefix(got, "reflect:") {
		t.Fatalf("MakeFunc panic missing reflect prefix: %s", got)
	}

	nextFalse := reflect.MakeFunc(reflect.TypeOf((func() bool)(nil)), func([]reflect.Value) []reflect.Value {
		return []reflect.Value{reflect.ValueOf(false)}
	})
	if got := reflect.ValueOf(reflectMakeFuncBoolLoop).Call([]reflect.Value{nextFalse})[0].Bool(); got {
		t.Fatalf("false MakeFunc loop result = %v, want false", got)
	}

	nextTrue := reflect.MakeFunc(reflect.TypeOf((func() bool)(nil)), func([]reflect.Value) []reflect.Value {
		return []reflect.Value{reflect.ValueOf(true)}
	})
	if got := reflect.ValueOf(reflectMakeFuncBoolLoop).Call([]reflect.Value{nextTrue})[0].Bool(); !got {
		t.Fatalf("true MakeFunc loop result = %v, want true", got)
	}
}
