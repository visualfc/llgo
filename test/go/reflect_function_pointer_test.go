package gotest

import (
	"encoding/json"
	"reflect"
	"testing"
	"unsafe"
)

type reflectPointerEncoder func() string

func (f reflectPointerEncoder) Encode() string { return f() }

func (f *reflectPointerEncoder) UnmarshalJSON(data []byte) error {
	var value string
	if err := json.Unmarshal(data, &value); err != nil {
		return err
	}
	*f = func() string { return value }
	return nil
}

type reflectPointerPlainFunc func() string

type reflectPointerOnlyFunc func()

func (*reflectPointerOnlyFunc) PointerOnly() {}

func TestReflectFunctionPointerOnlyMethod(t *testing.T) {
	// No value method or statically used pointer value should be necessary
	// to retain the compiler-emitted pointer method descriptor.
	typ := reflect.TypeOf(reflectPointerOnlyFunc(nil))
	ptr := reflect.New(typ)
	if typ.NumMethod() != 0 || reflect.PointerTo(typ).NumMethod() != 1 {
		t.Fatal("incorrect value or pointer method set")
	}
	u, ok := ptr.Interface().(interface{ PointerOnly() })
	if !ok {
		t.Fatal("reflected pointer lost its pointer-only method")
	}
	u.PointerOnly()
}

func TestReflectFunctionPointerIdentity(t *testing.T) {
	for _, ptr := range []any{
		new(reflectPointerEncoder),
		new(reflectPointerPlainFunc),
		new(func() string),
	} {
		value := reflect.ValueOf(ptr).Elem()
		typ := value.Type()
		want := reflect.TypeOf(ptr)
		for name, got := range map[string]reflect.Type{
			"PointerTo": reflect.PointerTo(typ),
			"Addr":      value.Addr().Type(),
			"New":       reflect.New(typ).Type(),
			"NewAt":     reflect.NewAt(typ, value.Addr().UnsafePointer()).Type(),
		} {
			if got != want || got.Elem() != typ {
				t.Errorf("%s(%v) = %v, want canonical %v with element %v", name, typ, got, want, typ)
			}
		}
		if got := reflect.TypeOf(value.Addr().Interface()); got != want {
			t.Errorf("Addr().Interface() type = %v, want %v", got, want)
		}
	}

	var f reflectPointerEncoder
	want := reflect.TypeOf(&f)
	for name, got := range map[string]reflect.Type{
		"parameter": reflect.TypeOf(func(*reflectPointerEncoder) {}).In(0),
		"field":     reflect.TypeOf(struct{ F *reflectPointerEncoder }{}).Field(0).Type,
		"slice":     reflect.TypeOf([]*reflectPointerEncoder{}).Elem(),
		"map":       reflect.TypeOf(map[int]*reflectPointerEncoder{}).Elem(),
	} {
		if got != want {
			t.Errorf("%s pointer type = %v, want canonical %v", name, got, want)
		}
	}
	if got, want := reflect.PointerTo(want), reflect.TypeOf((**reflectPointerEncoder)(nil)); got != want {
		t.Errorf("pointer to function pointer = %v, want canonical %v", got, want)
	}
}

func TestReflectFunctionPointerMethods(t *testing.T) {
	var f reflectPointerEncoder
	typ := reflect.TypeOf(f)
	unmarshaler := reflect.TypeOf((*json.Unmarshaler)(nil)).Elem()
	if typ.Implements(unmarshaler) {
		t.Fatal("value type unexpectedly implements pointer-receiver interface")
	}
	for name, ptr := range map[string]reflect.Value{
		"Addr":  reflect.ValueOf(&f).Elem().Addr(),
		"New":   reflect.New(typ),
		"NewAt": reflect.NewAt(typ, unsafe.Pointer(&f)),
	} {
		if got := ptr.Type().NumMethod(); got != 2 {
			t.Fatalf("%s pointer has %d methods, want 2", name, got)
		}
		if !ptr.Type().Implements(unmarshaler) {
			t.Fatalf("%s pointer does not implement json.Unmarshaler", name)
		}
		u, ok := ptr.Interface().(json.Unmarshaler)
		if !ok {
			t.Fatalf("%s pointer interface assertion failed", name)
		}
		if err := u.UnmarshalJSON([]byte(`"interface"`)); err != nil {
			t.Fatal(err)
		}
		if got := ptr.MethodByName("Encode").Call(nil)[0].String(); got != "interface" {
			t.Fatalf("%s inherited value method returned %q", name, got)
		}
		out := ptr.MethodByName("UnmarshalJSON").Call([]reflect.Value{reflect.ValueOf([]byte(`"method"`))})
		if !out[0].IsNil() || ptr.Elem().Interface().(reflectPointerEncoder)() != "method" {
			t.Fatalf("%s reflected pointer method did not update the function", name)
		}
	}
}

func TestReflectFunctionPointerJSON(t *testing.T) {
	var config struct{ Encoder reflectPointerEncoder }
	if err := json.Unmarshal([]byte(`{"Encoder":"iso8601"}`), &config); err != nil {
		t.Fatal(err)
	}
	if got := config.Encoder(); got != "iso8601" {
		t.Fatalf("decoded encoder returned %q", got)
	}
}

func TestReflectDynamicFunctionPointer(t *testing.T) {
	typ := reflect.FuncOf(nil, []reflect.Type{reflect.TypeOf("")}, false)
	ptr := reflect.New(typ)
	want := reflect.TypeOf((*func() string)(nil))
	if ptr.Type() != want || reflect.PointerTo(typ) != want {
		t.Fatalf("dynamic function pointer = %v, want canonical %v", ptr.Type(), want)
	}
	captured := "captured"
	ptr.Elem().Set(reflect.ValueOf(func() string { return captured }))
	f, ok := ptr.Interface().(*func() string)
	if !ok || (*f)() != captured {
		t.Fatal("dynamic function pointer did not preserve its closure")
	}
}

func TestReflectDynamicFunctionPointerConcurrent(t *testing.T) {
	// This signature and its pointer types have no static descriptors.
	array := reflect.ArrayOf(13, reflect.TypeOf(""))
	typ := reflect.FuncOf([]reflect.Type{array}, []reflect.Type{array}, false)
	const workers = 16
	results := make(chan reflect.Type, workers)
	for i := 0; i < workers; i++ {
		go func(i int) {
			if i%2 == 0 {
				results <- reflect.PointerTo(typ)
			} else {
				results <- reflect.New(typ).Type()
			}
		}(i)
	}
	want := <-results
	for i := 1; i < workers; i++ {
		if got := <-results; got != want {
			t.Fatalf("concurrent function pointers are not canonical: %v != %v", got, want)
		}
	}
	ptr := reflect.New(typ)
	if want.Elem() != typ || ptr.Elem().Type() != typ || !ptr.Elem().IsNil() {
		t.Fatal("dynamic pointer does not describe a nil function of the requested type")
	}
	if got := reflect.New(want).Type(); got != reflect.PointerTo(want) || got.Elem() != want {
		t.Fatal("dynamic pointer-to-pointer identity was not preserved")
	}
}
