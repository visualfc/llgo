//go:build !baremetal && !wasm

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license.
// See LICENSES/Go-BSD-3-Clause.txt at this module root for license terms.

package runtime

import (
	_ "unsafe"

	psync "github.com/xgo-dev/llgo/runtime/internal/sync"
)

// This locally ports only the part of the Go runtime timer implementation that
// does not depend on the scheduler's private M/P, netpoll, and gopark ABIs. It
// deliberately stays in LLGo's replacement runtime instead of source-patching
// or additively compiling the upstream runtime package. Like runtime/time.go,
// it keeps timers in a 4-ary heap and advances periodic timers from their
// intended deadline instead of from callback completion.

const (
	timerHeapArity = 4
	maxTimerWhen   = int64(1<<63 - 1)
	// Bound one native wait so a deadline saturated at maxTimerWhen does not
	// overflow the platform timespec conversion.
	maxTimerCondWait = int64(24 * 60 * 60 * 1e9)
)

type timerState struct {
	r         *runtimeTimer
	callback  runtimeTimerCallback
	heapIndex int
	active    bool
}

var (
	timerSchedulerOnce psync.Once
	timerSchedulerMu   psync.Mutex
	timerSchedulerCond psync.Cond
	timerSchedulerHeap []*timerState
	timerSchedulerMap  map[*runtimeTimer]*timerState
)

func initTimerScheduler() {
	timerSchedulerMu.Init(nil)
	initTimerSchedulerCond()
	timerSchedulerMap = make(map[*runtimeTimer]*timerState)
	go timerSchedulerLoop()
}

func ensureTimerScheduler() {
	timerSchedulerOnce.Do(initTimerScheduler)
}

// timerSchedulerHeadLocked returns a value snapshot of the current head. The
// deadline must be copied separately because reset can mutate the head timer in
// place while timerSchedulerMu is held.
func timerSchedulerHeadLocked() (*timerState, int64) {
	if len(timerSchedulerHeap) == 0 {
		return nil, 0
	}
	st := timerSchedulerHeap[0]
	return st, st.r.when
}

func signalTimerSchedulerIfHeadChangedLocked(old *timerState, oldWhen int64) {
	current, currentWhen := timerSchedulerHeadLocked()
	if current != old || currentWhen != oldWhen {
		timerSchedulerCond.Signal()
	}
}

func startRuntimeTimer(r *runtimeTimer) {
	if r == nil {
		return
	}
	ensureTimerScheduler()
	timerSchedulerMu.Lock()
	oldHead, oldHeadWhen := timerSchedulerHeadLocked()
	st := timerSchedulerMap[r]
	if st == nil {
		st = &timerState{r: r, heapIndex: -1}
		timerSchedulerMap[r] = st
	} else if st.active {
		timerHeapRemove(st.heapIndex)
	}
	st.callback = snapshotRuntimeTimer(r)
	st.active = true
	timerHeapAdd(st)
	signalTimerSchedulerIfHeadChangedLocked(oldHead, oldHeadWhen)
	timerSchedulerMu.Unlock()
}

func stopRuntimeTimer(r *runtimeTimer) bool {
	if r == nil {
		return false
	}
	ensureTimerScheduler()
	timerSchedulerMu.Lock()
	oldHead, oldHeadWhen := timerSchedulerHeadLocked()
	st := timerSchedulerMap[r]
	wasActive := st != nil && st.active
	if wasActive {
		timerHeapRemove(st.heapIndex)
		st.active = false
		delete(timerSchedulerMap, r)
		signalTimerSchedulerIfHeadChangedLocked(oldHead, oldHeadWhen)
	}
	timerSchedulerMu.Unlock()
	return wasActive
}

// resetRuntimeTimer updates all fields while holding the scheduler lock.
// update is used only by the pre-Go 1.23 modTimer ABI, whose callback can also
// change during a reset.
func resetRuntimeTimer(r *runtimeTimer, when, period int64, update func()) bool {
	if r == nil {
		return false
	}
	ensureTimerScheduler()
	timerSchedulerMu.Lock()
	oldHead, oldHeadWhen := timerSchedulerHeadLocked()
	st := timerSchedulerMap[r]
	wasActive := st != nil && st.active
	if st == nil {
		st = &timerState{r: r, heapIndex: -1}
		timerSchedulerMap[r] = st
	} else if st.active {
		timerHeapRemove(st.heapIndex)
	}
	if update != nil {
		update()
	}
	r.when = when
	r.period = period
	st.callback = snapshotRuntimeTimer(r)
	st.active = true
	timerHeapAdd(st)
	signalTimerSchedulerIfHeadChangedLocked(oldHead, oldHeadWhen)
	timerSchedulerMu.Unlock()
	return wasActive
}

func timerSchedulerLoop() {
	timerSchedulerMu.Lock()
	for {
		if len(timerSchedulerHeap) == 0 {
			timerSchedulerCond.Wait(&timerSchedulerMu)
			continue
		}

		st := timerSchedulerHeap[0]
		now := runtimeNano()
		if wait := timerSchedulerWaitDuration(st.r.when, now); wait > 0 {
			timerSchedulerTimedWait(wait)
			continue
		}

		when := st.r.when
		period := st.r.period
		callback := st.callback
		timerHeapRemove(0)
		if period > 0 {
			st.r.when = timerNextWhen(when, period, now)
			timerHeapAdd(st)
		} else {
			st.active = false
			delete(timerSchedulerMap, st.r)
		}

		delay := now - when
		if delay < 0 {
			delay = 0
		}
		timerSchedulerMu.Unlock()
		callback.run(delay)
		timerSchedulerMu.Lock()
	}
}

func timerSchedulerWaitDuration(when, now int64) int64 {
	if when <= now {
		return 0
	}
	wait := when - now
	if wait <= 0 || wait > maxTimerCondWait {
		return maxTimerCondWait
	}
	return wait
}

func timerNextWhen(when, period, now int64) int64 {
	delay := now - when
	if delay < 0 {
		delay = 0
	}
	next := when + period*(1+delay/period)
	if next < 0 {
		next = maxTimerWhen
	}
	return next
}

//go:linkname timeSleep time.Sleep
func timeSleep(ns int64) {
	if ns <= 0 {
		return
	}
	when := runtimeNano() + ns
	if when < 0 {
		when = maxTimerWhen
	}
	done := make(chan struct{}, 1)
	r := &runtimeTimer{
		when: when,
		f:    timeSleepWake,
		arg:  done,
	}
	startRuntimeTimer(r)
	<-done
}

func timerHeapLess(i, j int) bool {
	return timerSchedulerHeap[i].r.when < timerSchedulerHeap[j].r.when
}

func timerHeapSwap(i, j int) {
	timerSchedulerHeap[i], timerSchedulerHeap[j] = timerSchedulerHeap[j], timerSchedulerHeap[i]
	timerSchedulerHeap[i].heapIndex = i
	timerSchedulerHeap[j].heapIndex = j
}

func timerHeapAdd(st *timerState) {
	st.heapIndex = len(timerSchedulerHeap)
	timerSchedulerHeap = append(timerSchedulerHeap, st)
	timerHeapSiftUp(st.heapIndex)
}

func timerHeapRemove(i int) {
	n := len(timerSchedulerHeap) - 1
	removed := timerSchedulerHeap[i]
	if i != n {
		timerHeapSwap(i, n)
	}
	timerSchedulerHeap[n] = nil
	timerSchedulerHeap = timerSchedulerHeap[:n]
	removed.heapIndex = -1
	if i != n && !timerHeapSiftDown(i) {
		timerHeapSiftUp(i)
	}
}

func timerHeapSiftUp(i int) {
	for i > 0 {
		p := (i - 1) / timerHeapArity
		if !timerHeapLess(i, p) {
			return
		}
		timerHeapSwap(i, p)
		i = p
	}
}

func timerHeapSiftDown(i int) bool {
	moved := false
	for {
		child := timerHeapArity*i + 1
		if child >= len(timerSchedulerHeap) {
			return moved
		}
		best := child
		limit := child + timerHeapArity
		if limit > len(timerSchedulerHeap) {
			limit = len(timerSchedulerHeap)
		}
		for candidate := child + 1; candidate < limit; candidate++ {
			if timerHeapLess(candidate, best) {
				best = candidate
			}
		}
		if !timerHeapLess(best, i) {
			return moved
		}
		timerHeapSwap(i, best)
		i = best
		moved = true
	}
}

//go:linkname time_now time.now
func time_now() (sec int64, nsec int32, mono int64) {
	sec, nsec = walltime()
	mono = runtimeNano()
	return
}

//go:linkname time_runtimeNow time.runtimeNow
func time_runtimeNow() (sec int64, nsec int32, mono int64) {
	return time_now()
}

//go:linkname time_runtimeNano time.runtimeNano
func time_runtimeNano() int64 {
	return runtimeNano()
}

//go:linkname time_runtimeIsBubbled time.runtimeIsBubbled
func time_runtimeIsBubbled() bool {
	return false
}
