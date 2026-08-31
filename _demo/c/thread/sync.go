//go:build !windows

package main

import (
	"io"
	"sync"
	"unsafe"

	llsync "github.com/goplus/lib/c/pthread/sync"
)

type synchronizedValue struct {
	mu sync.Mutex
	s  string
	i  int
	w  io.Writer
}

// Retain the Go mutex/C pthread mutex layout and interface-field path from the
// former syncdebug command without another independently linked executable.
func verifySyncLayout() {
	value := &synchronizedValue{s: "hello", i: 123, w: io.Discard}
	if unsafe.Sizeof(sync.Mutex{}) == 0 || unsafe.Sizeof(llsync.Mutex{}) == 0 {
		panic("mutex layout")
	}
	value.mu.Lock()
	if value.s != "hello" || value.i != 123 || value.w == nil {
		value.mu.Unlock()
		panic("synchronized value")
	}
	if _, err := value.w.Write([]byte(value.s)); err != nil {
		value.mu.Unlock()
		panic(err)
	}
	value.mu.Unlock()
}
