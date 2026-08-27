//go:build windows

package sync_test

import (
	"sync"
	"testing"
	"time"
)

func TestRWMutexHighContention(t *testing.T) {
	const (
		workers    = 256
		iterations = 6000
	)

	type lockedMap struct {
		mu sync.RWMutex
		m  map[int]int
	}
	values := lockedMap{m: make(map[int]int)}
	var wg sync.WaitGroup
	wg.Add(workers)
	for worker := 0; worker < workers; worker++ {
		go func(id int) {
			defer wg.Done()
			for i := 0; i < iterations; i++ {
				key := (id + i) & 15
				values.mu.RLock()
				_, ok := values.m[key]
				values.mu.RUnlock()
				if !ok || i&7 == 0 {
					values.mu.Lock()
					values.m[key] = i
					values.mu.Unlock()
				}
			}
		}(worker)
	}

	done := make(chan struct{})
	go func() {
		wg.Wait()
		close(done)
	}()
	select {
	case <-done:
	case <-time.After(30 * time.Second):
		t.Fatal("RWMutex contention did not make progress")
	}
}
