//go:build windows

/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	cliteos "github.com/xgo-dev/llgo/runtime/internal/clite/os"
	psync "github.com/xgo-dev/llgo/runtime/internal/sync"
	"github.com/xgo-dev/llgo/runtime/internal/sync/atomic"
	"github.com/xgo-dev/llgo/runtime/internal/thread"
)

// mOS is intentionally empty for the current detached 1:1 backend. As in the
// Go runtime, CreateThread owns the thread lifetime; LLGo does not retain a
// closed HANDLE in the scheduler object.
type mOS struct{}

// processExiting is non-zero after runtime.exit or syscall.Exit starts
// terminating the process. It serves the same purpose as exiting in the Go
// runtime. The thread backend separately records the same transition before
// Windows starts running FLS process-shutdown callbacks.
var processExiting uint32

// processExitLock is used only to freeze a thread until ExitProcess terminates
// it. Its zero value is a ready-to-use Windows SRW lock.
var processExitLock psync.Mutex

// ExitProcess marks the runtime as exiting before asking Windows to terminate
// all process threads. Keep every Go-facing process exit path behind this
// helper so newosproc can distinguish shutdown from a real resource failure.
//
//go:nosplit
func ExitProcess(code uint32) {
	atomic.Store(&processExiting, 1)
	thread.BeginProcessExit()
	cliteos.ExitProcess(code)
}

//go:linkname runtime_exit runtime.exit
//go:nosplit
func runtime_exit(code int32) {
	ExitProcess(uint32(code))
}

func newosproc(mp *m, stackSize uintptr) int {
	ret := int(thread.CreateDetached(
		stackSize,
		thread.RoutineFunc(mstart),
		c.Pointer(unsafe.Pointer(mp)),
	))
	if ret != 0 {
		handleThreadCreateFailureDuringExit()
	}
	return ret
}

func handleThreadCreateFailureDuringExit() {
	if atomic.Load(&processExiting) == 0 {
		return
	}

	// CreateThread may fail while ExitProcess is tearing down the process.
	// Match the Go runtime's handling of issue #18253: freeze this thread and
	// let the exiting thread finish instead of reporting a spurious panic.
	processExitLock.Lock()
	processExitLock.Lock()
}

func exitCurrentM() {
	mp := getg().m
	mexit(mp)
	thread.Exit()
}
