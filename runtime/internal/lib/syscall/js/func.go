// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license.
// See LICENSES/Go-BSD-3-Clause.txt at this module root for license terms.

//go:build js && wasm
// +build js,wasm

package js

import (
	"sync"

	"github.com/xgo-dev/llgo/runtime/internal/clite"
	llruntime "github.com/xgo-dev/llgo/runtime/internal/runtime"
)

var (
	funcsMu    sync.Mutex
	funcs             = make(map[uint32]func(Value, []Value) any)
	nextFuncID uint32 = 1
)

// Func is a wrapped Go function to be called by JavaScript.
type Func struct {
	Value // the JavaScript function that invokes the Go function
	id    uint32
}

// FuncOf returns a function to be used by JavaScript.
//
// The Go function fn is eventually called with the value of JavaScript's
// "this" keyword and the arguments of the invocation. In LLGo's Emscripten C
// profiles the host callback only queues an event and wakes the scheduler; Go
// dispatch happens later on a new goroutine, after the JavaScript bridge has
// returned. The JavaScript invocation therefore returns undefined rather than
// synchronously returning fn's result. This prevents a callback from reentering
// Go while an Asyncify context is suspended.
//
// Func.Release must be called to free up resources when the function will not be invoked any more.
func FuncOf(fn func(this Value, args []Value) any) Func {
	funcsMu.Lock()
	if len(funcs) == 0 {
		emval_install_invoke()
		llruntime.RegisterWasmCallbackPoll(pollCallbacks)
	}
	id := nextFuncID
	nextFuncID++
	funcs[id] = fn
	funcsMu.Unlock()
	var buf [20]byte
	sid := string(itoa(buf[:], uint64(id)))
	// A modularized Emscripten build intentionally has no global Module.
	// Capture the exported bridge from this module instance so callbacks work
	// in Node, browsers, and workers without leaking a process-global module.
	invoke := emval_get_module_property(c.Str("_llgo_invoke"))
	factory := functionConstructor.New(ValueOf("invoke"), ValueOf(`
		return function() {
			const event = { id:`+sid+`, this: this, args: arguments };
			invoke(event);
		};
	`))
	wrap := factory.Invoke(invoke)
	return Func{
		id:    id,
		Value: wrap,
	}
}

func itoa(buf []byte, val uint64) []byte {
	i := len(buf) - 1
	for val >= 10 {
		buf[i] = byte(val%10 + '0')
		i--
		val /= 10
	}
	buf[i] = byte(val + '0')
	return buf[i:]
}

// Release frees up resources allocated for the function.
// The function must not be invoked after calling Release.
// It is allowed to call Release while the function is still running.
func (c Func) Release() {
	funcsMu.Lock()
	delete(funcs, c.id)
	if len(funcs) == 0 {
		llruntime.RegisterWasmCallbackPoll(nil)
	}
	funcsMu.Unlock()
}

func dispatchCallback(handle uintptr) {
	defer cEmvalDecref(handle)
	cb := Value{ref: ref(handle)}
	id := uint32(cb.Get("id").Int())
	funcsMu.Lock()
	f, ok := funcs[id]
	funcsMu.Unlock()
	if !ok {
		Global().Get("console").Call("error", "call to released function")
		return
	}

	// Call the js.Func with arguments
	this := cb.Get("this")
	argsObj := cb.Get("args")
	args := make([]Value, argsObj.Length())
	for i := range args {
		args[i] = argsObj.Index(i)
	}
	result := f(this, args)

	// Return the result to js
	cb.Set("result", result)
}

func pollCallbacks() {
	// The host sets a byte in wasm memory when it enqueues the first event, so
	// an idle scheduler does not cross the wasm/JavaScript boundary merely to
	// inspect an empty JavaScript array.
	if !emval_has_pending_invoke() {
		return
	}
	for {
		handle := emval_take_pending_invoke()
		if handle == 0 {
			return
		}
		go dispatchCallback(handle)
	}
}
