//go:build !nogc

package main

import "runtime"

type windowsGCProbe struct {
	value int
}

//go:noinline
func makeWindowsGCProbe(finalized chan<- int) {
	probe := &windowsGCProbe{value: 42}
	runtime.SetFinalizer(probe, func(value *windowsGCProbe) {
		finalized <- value.value
	})
}

func checkConcurrentGC() {
	const workers = 4
	ready := make(chan struct{}, workers)
	release := make(chan struct{})
	done := make(chan int, workers)
	finalized := make(chan int, workers)
	for worker := 0; worker < workers; worker++ {
		go func(id int) {
			probe := &windowsGCProbe{value: 40 + id}
			runtime.SetFinalizer(probe, func(value *windowsGCProbe) {
				finalized <- value.value
			})
			ready <- struct{}{}
			<-release
			value := probe.value
			if value != 40+id {
				panic("Windows GC corrupted a worker stack root")
			}
			runtime.KeepAlive(probe)
			done <- value
		}(worker)
	}
	for worker := 0; worker < workers; worker++ {
		<-ready
	}
	runtime.GC()
	select {
	case <-finalized:
		panic("Windows GC finalized a live worker stack root")
	default:
	}
	close(release)
	want := 40*workers + workers*(workers-1)/2
	got := 0
	for worker := 0; worker < workers; worker++ {
		got += <-done
	}
	if got != want {
		panic("Windows GC worker roots returned corrupt values")
	}
}

func checkGC() {
	checkConcurrentGC()

	finalized := make(chan int, 1)
	created := make(chan struct{})
	go func() {
		makeWindowsGCProbe(finalized)
		close(created)
	}()
	<-created

	var before runtime.MemStats
	runtime.ReadMemStats(&before)
	for attempt := 0; attempt < 8; attempt++ {
		runtime.Gosched()
		runtime.GC()
		select {
		case value := <-finalized:
			if value != 42 {
				panic("Windows GC finalizer observed a corrupt object")
			}
			var after runtime.MemStats
			runtime.ReadMemStats(&after)
			if after.NumGC <= before.NumGC {
				panic("Windows runtime.GC did not advance MemStats.NumGC")
			}
			return
		default:
		}
	}
	panic("Windows GC did not run the finalizer")
}
