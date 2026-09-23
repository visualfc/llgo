package main

import (
	"runtime"
	"sync"
	"sync/atomic"
	"time"
	_ "unsafe"
)

//go:linkname gmpForTesting github.com/xgo-dev/llgo/runtime/internal/runtime.GMPForTesting
func gmpForTesting() (goid, parentGoid uint64, mid int64, pid int32, gstatus, pstatus uint32, linked bool)

//llgo:gls
var workerLocalState *int

type workerIdentity struct {
	mid    int64
	pid    int32
	thread uintptr
}

func main() {
	testParallelWorkers()
	testPinnedGoroutine()
	testBoundedWorkerLifecycle()
	testCrossWorkerChannelHandoffs()
	testCrossWorkerSynchronization()
	testInterleavedWorkerLocality()
	testCrossWorkerTimerWake()
	println("wasm workers ok")
}

func testParallelWorkers() {
	var identities [2]workerIdentity
	record := func() {
		_, _, mid, pid, gstatus, pstatus, linked := gmpForTesting()
		if gstatus != 2 || pstatus != 1 || !linked {
			panic("invalid worker G/M/P state")
		}
		slot := parallelWorkerBarrier()
		if slot < 0 || int(slot) >= len(identities) {
			panic("parallel worker barrier timed out")
		}
		identities[slot] = workerIdentity{
			mid:    mid,
			pid:    pid,
			thread: parallelWorkerThread(slot),
		}
	}

	done := make(chan struct{})
	go func() {
		record()
		close(done)
	}()
	record()
	<-done

	left, right := identities[0], identities[1]
	if left.mid == right.mid || left.pid == right.pid {
		panic("goroutines did not run on distinct scheduler workers")
	}
	if left.thread == 0 || right.thread == 0 || left.thread == right.thread {
		panic("goroutines did not overlap on distinct pthreads")
	}
}

func testPinnedGoroutine() {
	done := make(chan struct{})
	go func() {
		_, _, mid, pid, _, _, linked := gmpForTesting()
		thread := currentWorkerThread()
		if !linked || thread == 0 {
			panic("invalid initial worker identity")
		}
		for range 32 {
			runtime.Gosched()
		}
		time.Sleep(time.Millisecond)
		_, _, currentMid, currentPid, _, _, currentLinked := gmpForTesting()
		if !currentLinked || currentMid != mid || currentPid != pid || currentWorkerThread() != thread {
			panic("started goroutine migrated between workers")
		}
		close(done)
	}()
	<-done
}

func testBoundedWorkerLifecycle() {
	const goroutines = 2_000
	workerCount := int(configuredWorkerCount())
	var (
		done   atomic.Uint32
		mid    int64
		thread uintptr
	)
	mids := make(map[int64]struct{})
	threads := make(map[uintptr]struct{})
	for i := uint32(1); i <= goroutines; i++ {
		go func() {
			_, _, currentMID, _, _, _, linked := gmpForTesting()
			currentThread := currentWorkerThread()
			if !linked || currentThread == 0 {
				panic("invalid lifecycle worker identity")
			}
			mid = currentMID
			thread = currentThread
			done.Store(i)
		}()
		for done.Load() != i {
			runtime.Gosched()
		}
		mids[mid] = struct{}{}
		threads[thread] = struct{}{}
	}
	if len(mids) != workerCount || len(threads) != workerCount {
		panic("goroutine lifecycle escaped the bounded worker pool")
	}
}

func testCrossWorkerChannelHandoffs() {
	const handoffs = 20_000
	values := make(chan int)
	done := make(chan struct{})
	go func() {
		for i := range handoffs {
			values <- i
		}
		close(done)
	}()
	for i := range handoffs {
		if value := <-values; value != i {
			panic("cross-worker channel handoff lost ordering")
		}
	}
	<-done
}

func testCrossWorkerSynchronization() {
	workerCount := int(configuredWorkerCount())
	goroutines := workerCount * 2
	var (
		counter atomic.Uint32
		mu      sync.Mutex
		wg      sync.WaitGroup
	)
	values := make(chan int, goroutines)
	workers := make(map[int64]int)
	wg.Add(goroutines)
	for i := range goroutines {
		go func() {
			_, _, mid, _, _, _, _ := gmpForTesting()
			mu.Lock()
			counter.Add(1)
			workers[mid]++
			mu.Unlock()
			values <- i
			wg.Done()
		}()
	}
	wg.Wait()
	close(values)

	seen := 0
	for range values {
		seen++
	}
	if seen != goroutines || counter.Load() != uint32(goroutines) {
		panic("cross-worker synchronization lost work")
	}
	if len(workers) != workerCount {
		panic("goroutines did not use the bounded worker pool")
	}
}

type localityWaiter struct {
	mid     int64
	release chan struct{}
	done    chan struct{}
}

func testInterleavedWorkerLocality() {
	workerCount := int(configuredWorkerCount())
	ready := make(chan localityWaiter, workerCount*2)
	startBatch := func() {
		for range workerCount {
			release := make(chan struct{})
			done := make(chan struct{})
			go func() {
				if workerLocalState == nil {
					value := 1
					workerLocalState = &value
				}
				if *workerLocalState != 1 {
					panic("worker-local state was corrupted")
				}
				_, _, mid, _, _, _, _ := gmpForTesting()
				ready <- localityWaiter{mid: mid, release: release, done: done}
				<-release
				close(done)
			}()
		}
	}

	byWorker := make(map[int64][]localityWaiter, workerCount)
	for range 2 {
		startBatch()
		for range workerCount {
			waiter := <-ready
			byWorker[waiter.mid] = append(byWorker[waiter.mid], waiter)
		}
	}
	if len(byWorker) != workerCount {
		panic("locality test did not cover every worker")
	}
	for _, waiters := range byWorker {
		if len(waiters) != 2 {
			panic("locality test did not interleave two goroutines per worker")
		}
		close(waiters[0].release)
		<-waiters[0].done
		close(waiters[1].release)
		<-waiters[1].done
	}
}

func testCrossWorkerTimerWake() {
	done := make(chan struct{})
	go func() {
		time.Sleep(5 * time.Millisecond)
		close(done)
	}()
	select {
	case <-done:
	case <-time.After(time.Second):
		panic("timer did not wake a worker")
	}
}
