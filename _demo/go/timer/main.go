package main

import "time"

// Exercise clock, Duration, timer stop/reset and AfterFunc in one case. Long
// deadlines are stopped; only channel completion is used as synchronization.
func main() {
	start := time.Now()
	if start.String() == "" {
		panic("time string")
	}
	future := start.Add(time.Hour)
	if time.Until(future) <= 0 || time.Since(start) < 0 {
		panic("clock arithmetic")
	}

	t := time.NewTimer(time.Hour)
	if !t.Stop() {
		panic("new timer already fired")
	}
	t.Reset(time.Millisecond)
	<-t.C
	<-time.After(0)

	done := make(chan struct{})
	after := time.AfterFunc(time.Millisecond, func() { close(done) })
	<-done
	if after.Stop() {
		panic("AfterFunc reported stopped after completion")
	}
	println("timer ok")
}
