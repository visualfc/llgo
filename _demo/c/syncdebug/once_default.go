//go:build !windows

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
}
