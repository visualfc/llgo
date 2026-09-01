//go:build llgo && wasm && !(wasip1 && llgo.wasi_threads)

// Copyright (c) 2026 The XGo Authors. All rights reserved.
// Use of this source code is governed by the Apache License 2.0.

package runtime

import (
	"github.com/xgo-dev/llgo/runtime/internal/wasmevent"
)

var (
	wasmPollTimersHook   func()
	wasmTimerWaitHook    func() (wait uint64, active bool)
	wasmCallbackPollHook func()
)

// RegisterWasmCallbackPoll connects a host callback source to the logical
// scheduler. Host bridges only queue callbacks and wake the host wait; the Go
// callbacks themselves are started as ordinary Gs from this poll point.
func RegisterWasmCallbackPoll(poll func()) {
	wasmCallbackPollHook = poll
}

// RegisterWasmTimerHooks connects the Go-derived timer heap when the standard
// runtime package is linked. Programs that do not use timers keep the nil
// hooks and remain linkable without pulling in timer state.
func RegisterWasmTimerHooks(poll func(), wait func() (uint64, bool)) {
	wasmPollTimersHook = poll
	wasmTimerWaitHook = wait
}

func popWasmRunq() *g {
	if wasmCallbackPollHook != nil {
		wasmCallbackPollHook()
	}
	if wasmPollTimersHook != nil {
		wasmPollTimersHook()
	}
	return wasmSched.runq.Pop()
}

func waitWasmRunq() *g {
	for {
		if gp := popWasmRunq(); gp != nil {
			return gp
		}
		if wasmTimerWaitHook == nil {
			return nil
		}
		wait, active := wasmTimerWaitHook()
		if !active {
			return nil
		}
		if wait != 0 {
			wasmevent.Wait(wait)
		}
	}
}
