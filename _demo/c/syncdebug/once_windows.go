//go:build windows

package main

import llsync "github.com/goplus/lib/c/pthread/sync"

func testOnce() {
	var once llsync.Once
	onceCount := 0
	delta := 2
	if once.Do(func() { onceCount += delta }) != 0 || once.Do(func() { onceCount++ }) != 0 {
		panic("once failed")
	}
	if onceCount != 2 {
		panic("once ran more than once")
	}
}
