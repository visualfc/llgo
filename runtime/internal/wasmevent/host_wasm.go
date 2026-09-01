//go:build llgo && wasm && !(wasip1 && llgo.wasi_threads)

// Copyright (c) 2026 The XGo Authors. All rights reserved.
// Use of this source code is governed by the Apache License 2.0.

// Package wasmevent is the thin host-wait boundary used by the single-worker
// WebAssembly scheduler. Timer ordering remains in the Go-derived runtime heap;
// this package only yields to the selected Emscripten or WASI host.
package wasmevent

import _ "unsafe"

const LLGoFiles = "_wrap/host_wasm.c"

//go:linkname hostWait C.llgo_wasm_host_wait
func hostWait(nanoseconds uint64)

// Wait returns control to the host until the requested duration has elapsed.
func Wait(nanoseconds uint64) {
	hostWait(nanoseconds)
}
