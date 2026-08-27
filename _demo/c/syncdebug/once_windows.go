//go:build windows

package main

import llsync "github.com/goplus/lib/c/pthread/sync"

func testOnce() {
	var once llsync.Once
	onceCount := 0
	delta := 2
	// The Windows wrapper owns a callback bridge that preserves LLGo's closure
	// environment, so exercise the stronger capturing-closure path here.
	if once.Do(func() { onceCount += delta }) != 0 || once.Do(func() { onceCount++ }) != 0 {
		panic("once failed")
	}
	if onceCount != 2 {
		panic("once ran more than once")
	}
}
