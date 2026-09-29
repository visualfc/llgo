//go:build !nogc

package main

import "runtime"

func verifyWorkerGC() {
	var before, after runtime.MemStats
	runtime.ReadMemStats(&before)
	runtime.GC()
	runtime.ReadMemStats(&after)
	if after.NumGC <= before.NumGC {
		panic("initial Goexit thread prevented worker GC")
	}
}
