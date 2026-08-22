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

// Package sync exposes the synchronization primitives used by the hosted
// runtime, backed by pthreads on Unix and Win32 on Windows.
package sync

import (
	_ "unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

const (
	LLGoFiles = "_wrap/sync_windows.c"
	// MinGW exposes the address-wait imports used by this backend through
	// libsynchronization rather than libkernel32.
	LLGoPackage = "link: -lsynchronization"
)

// Once has the layout of Windows INIT_ONCE. Its zero value is ready for use.
type Once struct {
	state uintptr
}

//go:linkname winOnce C.llgo_win_once
func winOnce(once *Once, f *func()) c.Int

//export llgo_win_once_invoke
func llgo_win_once_invoke(f *func()) {
	(*f)()
}

func (o *Once) Do(f func()) c.Int {
	return winOnce(o, &f)
}

type MutexAttr struct{}

// Mutex has the layout of Windows SRWLOCK. Its zero value is ready for use.
type Mutex struct {
	state uintptr
}

//go:linkname winMutexLock C.llgo_win_mutex_lock
func winMutexLock(m *Mutex)

//go:linkname winMutexUnlock C.llgo_win_mutex_unlock
func winMutexUnlock(m *Mutex)

func (m *Mutex) Init(_ *MutexAttr) c.Int {
	m.state = 0
	return 0
}

func (m *Mutex) Destroy() {}

func (m *Mutex) Lock() {
	winMutexLock(m)
}

func (m *Mutex) Unlock() {
	winMutexUnlock(m)
}

type CondAttr struct{}

// Cond has the layout of Windows CONDITION_VARIABLE. Its zero value is ready
// for use with a Mutex backed by an exclusive SRW lock.
type Cond struct {
	state uintptr
}

//go:linkname winCondSignal C.llgo_win_cond_signal
func winCondSignal(cond *Cond) c.Int

//go:linkname winCondBroadcast C.llgo_win_cond_broadcast
func winCondBroadcast(cond *Cond) c.Int

//go:linkname winCondWait C.llgo_win_cond_wait
func winCondWait(cond *Cond, m *Mutex) c.Int

func (cond *Cond) Init(_ *CondAttr) c.Int {
	cond.state = 0
	return 0
}

func (cond *Cond) Destroy() {}

func (cond *Cond) Signal() c.Int {
	return winCondSignal(cond)
}

func (cond *Cond) Broadcast() c.Int {
	return winCondBroadcast(cond)
}

func (cond *Cond) Wait(m *Mutex) c.Int {
	return winCondWait(cond, m)
}

//go:linkname winWaitUint32 C.llgo_win_wait_uint32
func winWaitUint32(addr *uint32, value uint32) c.Int

//go:linkname winWakeUint32 C.llgo_win_wake_uint32
func winWakeUint32(addr *uint32)

// WaitUint32 blocks while addr still contains value. Callers must recheck the
// value after it returns because Windows permits spurious wakeups.
func WaitUint32(addr *uint32, value uint32) c.Int {
	return winWaitUint32(addr, value)
}

// WakeUint32 wakes one thread waiting for addr.
func WakeUint32(addr *uint32) {
	winWakeUint32(addr)
}
