package main

import llsync "github.com/goplus/lib/c/pthread/sync"

var (
	onceCount int
	onceDelta int
)

func addOnceDelta()  { onceCount += onceDelta }
func incrementOnce() { onceCount++ }

func testOnce() {
	var once llsync.Once
	onceCount, onceDelta = 0, 2
	if once.Do(addOnceDelta) != 0 || once.Do(incrementOnce) != 0 {
		panic("once failed")
	}
	if onceCount != 2 {
		panic("once ran more than once")
	}

	var closureOnce llsync.Once
	closureCount, closureDelta := 0, 3
	if closureOnce.DoFunc(func() { closureCount += closureDelta }) != 0 ||
		closureOnce.DoFunc(func() { closureCount++ }) != 0 {
		panic("closure once failed")
	}
	if closureCount != 3 {
		panic("closure once ran more than once")
	}

	var concurrentOnce llsync.Once
	concurrentValue := 0
	done := make(chan struct{}, 4)
	for value := 1; value <= 4; value++ {
		value := value
		go func() {
			if concurrentOnce.DoFunc(func() { concurrentValue = value }) != 0 {
				panic("concurrent closure once failed")
			}
			done <- struct{}{}
		}()
	}
	for i := 0; i < 4; i++ {
		<-done
	}
	if concurrentValue < 1 || concurrentValue > 4 {
		panic("concurrent closure once did not run")
	}

	var outerOnce, innerOnce llsync.Once
	nestedValue := 0
	if outerOnce.DoFunc(func() {
		if innerOnce.DoFunc(func() { nestedValue = 5 }) != 0 {
			panic("nested inner once failed")
		}
	}) != 0 {
		panic("nested outer once failed")
	}
	if nestedValue != 5 {
		panic("nested closure once did not run")
	}
}
