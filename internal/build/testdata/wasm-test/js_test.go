//go:build js && wasm

package wasmtest

import (
	"syscall/js"
	"testing"
	"time"
)

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
