//go:build js && wasm

package wasmtest

import (
	"reflect"
	"syscall/js"
	"testing"
	"time"
)

func TestReflectCallCanSleep(t *testing.T) {
	delayed := func() {
		time.Sleep(time.Millisecond)
		println("woke")
	}
	reflect.ValueOf(delayed).Call(nil)
}

func TestReflectCallComplex128(t *testing.T) {
	add := func(v complex128) complex128 {
		return v + complex(1, 2)
	}
	got := reflect.ValueOf(add).Call([]reflect.Value{reflect.ValueOf(complex(3, 4))})
	if len(got) != 1 || got[0].Complex() != complex(4, 6) {
		t.Fatalf("Call(add) = %v, want [(4+6i)]", got)
	}
}

func TestReflectMakeFuncComplex64(t *testing.T) {
	fn := reflect.MakeFunc(reflect.TypeOf((func(complex64) complex64)(nil)), func(args []reflect.Value) []reflect.Value {
		v := args[0].Complex()
		return []reflect.Value{reflect.ValueOf(complex64(v + complex(1, 2)))}
	})
	got := fn.Call([]reflect.Value{reflect.ValueOf(complex64(complex(3, 4)))})
	if len(got) != 1 || got[0].Complex() != complex(4, 6) {
		t.Fatalf("MakeFunc(complex64) = %v, want [(4+6i)]", got)
	}
}

func TestJSValueZeroIsUndefined(t *testing.T) {
	var value js.Value
	if !value.IsUndefined() || value.Type() != js.TypeUndefined {
		t.Fatalf("zero js.Value = undefined %v, type %v", value.IsUndefined(), value.Type())
	}
	if !value.Equal(js.Undefined()) {
		t.Fatal("zero js.Value does not equal js.Undefined()")
	}
}

func TestHostCallbackWakesScheduler(t *testing.T) {
	done := make(chan struct{}, 1)
	callback := js.FuncOf(func(js.Value, []js.Value) any {
		done <- struct{}{}
		return nil
	})
	defer callback.Release()

	js.Global().Call("setTimeout", callback, 0)
	select {
	case <-done:
	case <-time.After(time.Second):
		t.Fatal("JavaScript callback did not wake the scheduler")
	}
}

func TestHostCallbackCanBlock(t *testing.T) {
	done := make(chan int, 1)
	callback := js.FuncOf(func(js.Value, []js.Value) any {
		value := make(chan int)
		go func() {
			value <- 42
		}()
		got := <-value
		done <- got
		return got
	})
	defer callback.Release()

	js.Global().Call("setTimeout", callback, 0)
	select {
	case got := <-done:
		if got != 42 {
			t.Fatalf("callback result = %d, want 42", got)
		}
	case <-time.After(time.Second):
		t.Fatal("blocked JavaScript callback prevented another goroutine from running")
	}
}
