package main

import "time"

type void = [0]byte

type future[T any] interface {
	Then(func(T))
}

type callbackFuture[T any] struct {
	callback func(func(T))
}

func (f *callbackFuture[T]) Then(callback func(T)) {
	f.callback(callback)
}

func async[T any](callback func(func(T))) future[T] {
	return &callbackFuture[T]{callback: callback}
}

func runFuture[T any](f future[T]) T {
	var result T
	f.Then(func(value T) {
		result = value
	})
	return result
}

func timeout(d time.Duration) future[void] {
	return async(func(resolve func(void)) {
		go func() {
			time.Sleep(d)
			resolve(void{})
		}()
	})
}

// Preserve the generic Future interface and callback implementation from the
// former async demo, while explicitly waiting for its goroutine completion.
func testAsync() {
	if got := runFuture(async(func(resolve func(int)) { resolve(42) })); got != 42 {
		panic("synchronous future")
	}
	done := make(chan struct{})
	timeout(0).Then(func(void) { close(done) })
	<-done
}
