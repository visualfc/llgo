package wasmtest

import (
	"crypto/sha256"
	"fmt"
	"math"
	"reflect"
	"runtime"
	"testing"
	"time"
)

func TestStandardLibraryWasmAssembly(t *testing.T) {
	if got := math.Floor(3.75); got != 3 {
		t.Fatalf("math.Floor(3.75) = %v, want 3", got)
	}
	const wantSHA256 = "336154bf67f765f8f75d16a0accee61b5ee5f6a75b2a2905703df913bd550f3e"
	if got := fmt.Sprintf("%x", sha256.Sum256([]byte("wasm"))); got != wantSHA256 {
		t.Fatalf("sha256.Sum256(wasm) = %s, want %s", got, wantSHA256)
	}
}

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

func TestScheduler(t *testing.T) {
	done := make(chan int, 1)
	go func() {
		done <- 42
	}()

	select {
	case got := <-done:
		if got != 42 {
			t.Fatalf("goroutine result = %d, want 42", got)
		}
	case <-time.After(time.Second):
		t.Fatal("goroutine did not make progress")
	}
}

func TestPanicRecoverAndCaller(t *testing.T) {
	defer func() {
		got := recover()
		if got != "wasm-test-panic" {
			t.Fatalf("recover = %v, want wasm-test-panic", got)
		}
	}()

	if _, file, line, ok := runtime.Caller(0); !ok || file == "" || line == 0 {
		t.Fatalf("runtime.Caller = %q:%d, %v", file, line, ok)
	}
	panic("wasm-test-panic")
}
