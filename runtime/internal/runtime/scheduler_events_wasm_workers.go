//go:build llgo && js && wasm && llgo.wasm.workers

// Copyright (c) 2026 The XGo Authors. All rights reserved.
// Use of this source code is governed by the Apache License 2.0.

package runtime

var (
	wasmPollTimersHook   func()
	wasmTimerWaitHook    func() (wait uint64, active bool)
	wasmCallbackPollHook func()
)

// RegisterWasmCallbackPoll connects a host callback source to worker zero.
// Host bridges queue callbacks; worker zero turns them into ordinary Gs.
func RegisterWasmCallbackPoll(poll func()) {
	wasmCallbackPollHook = poll
}

// RegisterWasmTimerHooks connects the Go-derived timer heap. The timer
// implementation serializes heap access across workers.
func RegisterWasmTimerHooks(poll func(), wait func() (uint64, bool)) {
	wasmPollTimersHook = poll
	wasmTimerWaitHook = wait
}

func pollWasmEvents() {
	if wasmCallbackPollHook != nil {
		wasmCallbackPollHook()
	}
	if wasmPollTimersHook != nil {
		wasmPollTimersHook()
	}
}

// WakeWasmScheduler interrupts worker zero after another worker publishes an
// earlier timer or host work.
func WakeWasmScheduler() {
	wakeWasmEventWorker()
}
