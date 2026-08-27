//go:build !windows

package main

import llsync "github.com/goplus/lib/c/pthread/sync"

var (
	onceCount int
	onceDelta int
)

// POSIX pthread_once accepts a bare C function pointer, so keep these
// callbacks non-capturing. Passing a Go closure would supply the separate
// {function, environment} value used by LLGo and violate that C prototype.
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
