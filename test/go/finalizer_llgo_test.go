//go:build llgo && !baremetal && !nogc

package gotest

import (
	"runtime"
	"testing"
	"time"
	_ "unsafe"
)

//go:linkname getBDWGCFinalizeOnDemand C.GC_get_finalize_on_demand
func getBDWGCFinalizeOnDemand() int32

//go:linkname setBDWGCFinalizeOnDemand C.GC_set_finalize_on_demand
func setBDWGCFinalizeOnDemand(enabled int32)

func TestRuntimeAddCleanupStop(t *testing.T) {
	old := getBDWGCFinalizeOnDemand()
	setBDWGCFinalizeOnDemand(1)
	t.Cleanup(func() {
		setBDWGCFinalizeOnDemand(old)
	})

	const n = 32
	stopped := make(chan int32, n)
	active := make(chan int32, n)
	created := make(chan struct{})
	go func() {
		for i := range int32(n) {
			stoppedObject := new([64]byte)
			cleanup := runtime.AddCleanup(stoppedObject, func(value int32) {
				stopped <- value
			}, i)
			cleanup.Stop()
			runtime.KeepAlive(stoppedObject)

			activeObject := new([64]byte)
			runtime.AddCleanup(activeObject, func(value int32) {
				active <- value
			}, i)
		}
		close(created)
	}()
	<-created

	deadline := time.After(3 * time.Second)
	for len(active) <= n/2 {
		runtime.Gosched()
		runGCWithTimeout(t)
		select {
		case <-deadline:
			t.Fatalf("only %d/%d active cleanups ran", len(active), n)
		default:
		}
	}
	for range 3 {
		runGCWithTimeout(t)
	}
	if got := len(stopped); got != 0 {
		t.Fatalf("%d stopped cleanups ran", got)
	}
}

func TestRuntimeGCDrainsBDWGCFinalizersOnDemand(t *testing.T) {
	// BDWGC normally may invoke ready finalizers during a later allocation.
	// On-demand mode makes runtime.GC's explicit drain observable without
	// relying on allocation timing. This setting is process-global, so this
	// test and the neighboring finalizer tests must remain sequential.
	old := getBDWGCFinalizeOnDemand()
	setBDWGCFinalizeOnDemand(1)
	t.Cleanup(func() {
		setBDWGCFinalizeOnDemand(old)
	})

	const n = 32
	finalized := make(chan int32, n)
	created := make(chan struct{})
	go func() {
		makeFinalizerTinyObjects(n, finalized)
		close(created)
	}()
	<-created

	// InvokeFinalizers runs synchronously once BDWGC has queued an object.
	// The retries only let the producer goroutine exit so that conservative
	// stack and register roots no longer keep the objects reachable.
	for range 8 {
		runtime.Gosched()
		runGCWithTimeout(t)
		if len(finalized) > n/2 {
			return
		}
	}
	if got := len(finalized); got <= n/2 {
		t.Fatalf("runtime.GC ran only %d/%d on-demand finalizers", got, n)
	}
}
