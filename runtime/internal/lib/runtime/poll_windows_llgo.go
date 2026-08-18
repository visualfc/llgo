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
	psync "github.com/xgo-dev/llgo/runtime/internal/clite/sync"
	ctime "github.com/xgo-dev/llgo/runtime/internal/clite/time"
)

// These values must match internal/poll/fd_poll_runtime.go.
const (
	pollNoError        = 0
	pollErrClosing     = 1
	pollErrTimeout     = 2
	pollErrNotPollable = 3
)

const (
	windowsPollRead = uint32(1 << iota)
	windowsPollWrite
)

// windowsOverlapped matches syscall.Overlapped. windowsPollOperation matches
// the prefix of internal/poll.operation used by Go's Windows netpoller.
type windowsOverlapped struct {
	internal     uintptr
	internalHigh uintptr
	offset       uint32
	offsetHigh   uint32
	event        uintptr
}

type windowsPollOperation struct {
	o          windowsOverlapped
	runtimeCtx uintptr
	mode       int32
}

// LLGo currently maps goroutines to native threads instead of Go's M:N
// scheduler. Completion packets are therefore dispatched to per-descriptor
// condition variables; the official internal/poll package and its overlapped
// operation layout remain unchanged.
type windowsPollDesc struct {
	mu   psync.Mutex
	cond psync.Cond

	ready   uint32
	closing bool
	readDL  int64
	writeDL int64
}

var (
	windowsPollOnce  psync.Once
	windowsPollPort  uintptr
	windowsPollMapMu psync.Mutex
	windowsPollRoots map[uintptr]*windowsPollDesc
)

//go:linkname c_iocpCreate C.llgo_iocp_create
func c_iocpCreate(errno *uint32) uintptr

//go:linkname c_iocpAssociate C.llgo_iocp_associate
func c_iocpAssociate(port, handle, key uintptr, errno *uint32) c.Int

//go:linkname c_iocpGet C.llgo_iocp_get
func c_iocpGet(port uintptr, key *uintptr, overlapped *unsafe.Pointer, errno *uint32) c.Int

func windowsPollInit() {
	windowsPollMapMu.Init(nil)
	windowsPollRoots = make(map[uintptr]*windowsPollDesc)
	var errno uint32
	windowsPollPort = c_iocpCreate(&errno)
	if windowsPollPort == 0 {
		print("runtime: CreateIoCompletionPort failed (errno=", errno, ")\n")
		throw("runtime: netpollinit failed")
	}
	go windowsPollLoop()
}

func windowsPollLoop() {
	for {
		var key uintptr
		var overlapped unsafe.Pointer
		var errno uint32
		if c_iocpGet(windowsPollPort, &key, &overlapped, &errno) == 0 {
			print("runtime: GetQueuedCompletionStatus failed (errno=", errno, ")\n")
			throw("runtime: netpoll failed")
		}
		if key == 0 || overlapped == nil {
			continue
		}
		op := (*windowsPollOperation)(overlapped)
		if op.runtimeCtx != key {
			continue
		}
		bit := windowsPollModeBit(int(op.mode))
		if bit == 0 {
			continue
		}

		windowsPollMapMu.Lock()
		pd := windowsPollRoots[key]
		windowsPollMapMu.Unlock()
		if pd == nil {
			continue
		}
		pd.mu.Lock()
		pd.ready |= bit
		pd.cond.Broadcast()
		pd.mu.Unlock()
	}
}

func windowsPollModeBit(mode int) uint32 {
	switch mode {
	case 'r':
		return windowsPollRead
	case 'w':
		return windowsPollWrite
	default:
		return 0
	}
}

func windowsPollDescFromContext(ctx uintptr) *windowsPollDesc {
	if ctx == 0 {
		return nil
	}
	return (*windowsPollDesc)(unsafe.Pointer(ctx))
}

func windowsPollDeadline(pd *windowsPollDesc, mode int) int64 {
	if mode == 'r' {
		return pd.readDL
	}
	return pd.writeDL
}

func windowsWallDeadline(after int64) ctime.Timespec {
	seconds, nanoseconds := walltime()
	nsec := int64(nanoseconds) + after
	seconds += nsec / 1e9
	nsec %= 1e9
	return ctime.Timespec{Sec: ctime.TimeT(seconds), Nsec: c.Long(nsec)}
}

func windowsPollWait(ctx uintptr, mode int, canceled bool) int {
	pd := windowsPollDescFromContext(ctx)
	bit := windowsPollModeBit(mode)
	if pd == nil || bit == 0 {
		return pollErrNotPollable
	}

	pd.mu.Lock()
	defer pd.mu.Unlock()
	for {
		if pd.ready&bit != 0 {
			pd.ready &^= bit
			return pollNoError
		}
		if canceled {
			pd.cond.Wait(&pd.mu)
			continue
		}
		if pd.closing {
			return pollErrClosing
		}
		deadline := windowsPollDeadline(pd, mode)
		if deadline == 0 {
			pd.cond.Wait(&pd.mu)
			continue
		}
		remaining := deadline - runtimeNano()
		if remaining <= 0 {
			return pollErrTimeout
		}
		absolute := windowsWallDeadline(remaining)
		// Always re-check the monotonic deadline after a wake or timeout. This
		// also handles wall-clock adjustments while the condition wait runs.
		pd.cond.TimedWait(&pd.mu, &absolute)
	}
}

//go:linkname poll_runtime_pollServerInit internal/poll.runtime_pollServerInit
func poll_runtime_pollServerInit() {
	windowsPollOnce.Do(windowsPollInit)
}

//go:linkname poll_runtime_pollOpen internal/poll.runtime_pollOpen
func poll_runtime_pollOpen(fd uintptr) (uintptr, int) {
	windowsPollOnce.Do(windowsPollInit)
	pd := new(windowsPollDesc)
	pd.mu.Init(nil)
	pd.cond.Init(nil)
	ctx := uintptr(unsafe.Pointer(pd))
	var errno uint32
	if c_iocpAssociate(windowsPollPort, fd, ctx, &errno) == 0 {
		return 0, int(errno)
	}
	windowsPollMapMu.Lock()
	windowsPollRoots[ctx] = pd
	windowsPollMapMu.Unlock()
	return ctx, 0
}

//go:linkname poll_runtime_pollClose internal/poll.runtime_pollClose
func poll_runtime_pollClose(ctx uintptr) {
	pd := windowsPollDescFromContext(ctx)
	if pd == nil {
		return
	}
	pd.mu.Lock()
	pd.closing = true
	pd.cond.Broadcast()
	pd.mu.Unlock()
	windowsPollMapMu.Lock()
	delete(windowsPollRoots, ctx)
	windowsPollMapMu.Unlock()
}

//go:linkname poll_runtime_pollWait internal/poll.runtime_pollWait
func poll_runtime_pollWait(ctx uintptr, mode int) int {
	return windowsPollWait(ctx, mode, false)
}

//go:linkname poll_runtime_pollWaitCanceled internal/poll.runtime_pollWaitCanceled
func poll_runtime_pollWaitCanceled(ctx uintptr, mode int) {
	_ = windowsPollWait(ctx, mode, true)
}

//go:linkname poll_runtime_pollReset internal/poll.runtime_pollReset
func poll_runtime_pollReset(ctx uintptr, mode int) int {
	pd := windowsPollDescFromContext(ctx)
	if pd == nil || windowsPollModeBit(mode) == 0 {
		return pollErrNotPollable
	}
	pd.mu.Lock()
	defer pd.mu.Unlock()
	if pd.closing {
		return pollErrClosing
	}
	deadline := windowsPollDeadline(pd, mode)
	if deadline != 0 && deadline <= runtimeNano() {
		return pollErrTimeout
	}
	return pollNoError
}

//go:linkname poll_runtime_pollSetDeadline internal/poll.runtime_pollSetDeadline
func poll_runtime_pollSetDeadline(ctx uintptr, d int64, mode int) {
	pd := windowsPollDescFromContext(ctx)
	if pd == nil {
		return
	}
	var deadline int64
	if d != 0 {
		deadline = runtimeNano() + d
	}
	pd.mu.Lock()
	switch mode {
	case 'r':
		pd.readDL = deadline
	case 'w':
		pd.writeDL = deadline
	default:
		pd.readDL = deadline
		pd.writeDL = deadline
	}
	pd.cond.Broadcast()
	pd.mu.Unlock()
}

//go:linkname poll_runtime_pollUnblock internal/poll.runtime_pollUnblock
func poll_runtime_pollUnblock(ctx uintptr) {
	pd := windowsPollDescFromContext(ctx)
	if pd == nil {
		return
	}
	pd.mu.Lock()
	pd.closing = true
	pd.cond.Broadcast()
	pd.mu.Unlock()
}

//go:linkname poll_runtime_isPollServerDescriptor internal/poll.runtime_isPollServerDescriptor
func poll_runtime_isPollServerDescriptor(fd uintptr) bool {
	return windowsPollPort != 0 && fd == windowsPollPort
}
