package main

import (
	"sync"
	"sync/atomic"
)

type counter struct {
	mu sync.Mutex
	n  int32
}

func (c *counter) add() {
	c.mu.Lock()
	c.n++
	c.mu.Unlock()
}

// The barriers make goroutine completion deterministic. This retains the
// former goroutine, mutex, Once, WaitGroup, atomic and atomic-function-value
// calls in a single bounded concurrency owner.
func main() {
	const workers = 8
	start := make(chan struct{})
	var c counter
	var active int32
	var once sync.Once
	var initialized int32
	var wg sync.WaitGroup
	add := atomic.AddInt32

	for i := 0; i < workers; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			<-start
			once.Do(func() { initialized = 1 })
			c.add()
			add(&active, 1)
		}()
	}
	close(start)
	wg.Wait()
	if c.n != workers || atomic.LoadInt32(&active) != workers || initialized != 1 {
		panic("concurrency result")
	}
	println("concurrency ok")
}
