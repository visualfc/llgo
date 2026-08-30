//go:build windows

package runtime

import (
	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	ctime "github.com/xgo-dev/llgo/runtime/internal/clite/time"
)

func initTimerSchedulerCond() {
	if timerSchedulerCond.Init(nil) != 0 {
		panic("runtime: failed to initialize timer condition")
	}
}

func timerSchedulerTimedWait(wait int64) {
	sec, nsec := walltime()
	sec += wait / 1e9
	ns := int64(nsec) + wait%1e9
	if ns >= 1e9 {
		sec++
		ns -= 1e9
	}
	deadline := ctime.Timespec{Sec: ctime.TimeT(sec), Nsec: c.Long(ns)}
	timerSchedulerCond.TimedWait(&timerSchedulerMu, &deadline)
}
