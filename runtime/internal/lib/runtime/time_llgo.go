//go:build !go1.23 && !baremetal && !wasm

package runtime

import _ "unsafe"

// runtimeTimer must match time.runtimeTimer (and runtime.timer) layout.
type runtimeTimer struct {
	pp       uintptr
	when     int64
	period   int64
	f        func(any, uintptr)
	arg      any
	seq      uintptr
	nextwhen int64
	status   uint32
}

func snapshotRuntimeTimer(r *runtimeTimer) func(int64) {
	f, arg, seq := r.f, r.arg, r.seq
	return func(_ int64) {
		f(arg, seq)
	}
}

func startTimer(r *runtimeTimer) {
	startRuntimeTimer(r)
}

func stopTimer(r *runtimeTimer) bool {
	return stopRuntimeTimer(r)
}

func resetTimer(r *runtimeTimer, when int64) bool {
	return resetRuntimeTimer(r, when, r.period, nil)
}

func modTimer(r *runtimeTimer, when, period int64, f func(any, uintptr), arg any, seq uintptr) {
	resetRuntimeTimer(r, when, period, func() {
		r.f = f
		r.arg = arg
		r.seq = seq
	})
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
	stopRuntimeTimer(r)
}

func timeSleepWake(arg any, _ uintptr) {
	arg.(chan struct{}) <- struct{}{}
}
