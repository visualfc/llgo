package timerstress

import (
	"fmt"
	"os"
	"sync"
	"sync/atomic"
	"testing"
	"time"
)

func stressCount(t *testing.T, base int) int {
	t.Helper()
	switch profile := os.Getenv("LLGO_STRESS_PROFILE"); profile {
	case "", "default":
		return base
	case "quick":
		if count := base / 8; count > 0 {
			return count
		}
		return 1
	case "heavy":
		return base * 2
	default:
		t.Fatalf("unknown LLGO_STRESS_PROFILE %q", profile)
		return 0
	}
}

func waitGroup(work *sync.WaitGroup, timeout time.Duration) error {
	done := make(chan struct{})
	go func() {
		work.Wait()
		close(done)
	}()
	select {
	case <-done:
		return nil
	case <-time.After(timeout):
		return fmt.Errorf("workers did not finish within %s", timeout)
	}
}

func TestManyLiveTimersWithConcurrentStopReset(t *testing.T) {
	live := stressCount(t, 16384)
	workers := stressCount(t, 512)
	operations := stressCount(t, 1000)

	var unexpected atomic.Int64
	timers := make([]*time.Timer, live)
	for i := range timers {
		timers[i] = time.AfterFunc(time.Hour, func() {
			unexpected.Add(1)
		})
	}
	defer func() {
		for _, timer := range timers {
			timer.Stop()
		}
	}()

	start := make(chan struct{})
	var work sync.WaitGroup
	work.Add(workers)
	for worker := 0; worker < workers; worker++ {
		go func(seed uint32) {
			defer work.Done()
			<-start
			x := seed
			for operation := 0; operation < operations; operation++ {
				x ^= x << 13
				x ^= x >> 17
				x ^= x << 5
				timer := timers[int(x)%len(timers)]
				if operation&3 == 0 {
					timer.Stop()
					timer.Reset(time.Hour)
				} else {
					timer.Reset(time.Hour + time.Duration(x&1023)*time.Millisecond)
				}
			}
		}(uint32(worker + 1))
	}

	fired := make(chan struct{})
	target := time.AfterFunc(20*time.Millisecond, func() { close(fired) })
	defer target.Stop()
	close(start)
	select {
	case <-fired:
	case <-time.After(10 * time.Second):
		t.Fatal("short timer starved behind concurrent heap operations")
	}
	if err := waitGroup(&work, 2*time.Minute); err != nil {
		t.Fatal(err)
	}
	if got := unexpected.Load(); got != 0 {
		t.Fatalf("far-future timers fired %d times", got)
	}
}

func TestSharedTimerConcurrentStopReset(t *testing.T) {
	workers := stressCount(t, 512)
	operations := stressCount(t, 1000)

	var fired atomic.Int64
	timer := time.AfterFunc(time.Hour, func() { fired.Add(1) })
	defer timer.Stop()
	start := make(chan struct{})
	var work sync.WaitGroup
	work.Add(workers)
	for worker := 0; worker < workers; worker++ {
		go func(worker int) {
			defer work.Done()
			<-start
			for operation := 0; operation < operations; operation++ {
				if (worker+operation)&1 == 0 {
					timer.Stop()
				} else {
					timer.Reset(time.Hour + time.Duration(operation&1)*time.Second)
				}
			}
		}(worker)
	}
	close(start)
	if err := waitGroup(&work, 2*time.Minute); err != nil {
		t.Fatal(err)
	}
	timer.Stop()
	if got := fired.Load(); got != 0 {
		t.Fatalf("shared far-future timer fired %d times", got)
	}
}

func TestTimerCallbackBurst(t *testing.T) {
	callbacks := stressCount(t, 1024)
	delivered := make(chan struct{}, callbacks)
	for i := 0; i < callbacks; i++ {
		time.AfterFunc(20*time.Millisecond+time.Duration(i&15)*time.Microsecond, func() {
			delivered <- struct{}{}
		})
	}
	deadline := time.After(30 * time.Second)
	for received := 0; received < callbacks; received++ {
		select {
		case <-delivered:
		case <-deadline:
			t.Fatalf("received %d of %d timer callbacks", received, callbacks)
		}
	}
	select {
	case <-delivered:
		t.Fatal("timer callback delivered more than once")
	case <-time.After(50 * time.Millisecond):
	}
}

func TestConcurrentSleep(t *testing.T) {
	sleepers := stressCount(t, 512)
	start := make(chan struct{})
	var work sync.WaitGroup
	work.Add(sleepers)
	for i := 0; i < sleepers; i++ {
		go func() {
			defer work.Done()
			<-start
			time.Sleep(20 * time.Millisecond)
		}()
	}
	close(start)
	if err := waitGroup(&work, 30*time.Second); err != nil {
		t.Fatal(err)
	}
}
