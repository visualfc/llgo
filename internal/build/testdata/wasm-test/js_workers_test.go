//go:build js && wasm && llgo.wasm.workers

package wasmtest

import (
	"fmt"
	"os"
	"runtime"
	"syscall/js"
	"testing"
	"time"
	_ "unsafe"

	"github.com/xgo-dev/llgo/runtime/_test/workerlocality"
)

//go:linkname schedulerProcID github.com/xgo-dev/llgo/runtime/internal/runtime.SchedulerProcID
func schedulerProcID() int

type workerCallbackResult struct {
	origin   int
	callback int
	err      error
}

func TestWorkerTLSRoots(t *testing.T) {
	result := make(chan int, 1)
	go func() {
		workerlocality.InstallTLSValue(42)
		workerlocality.InstallTLSFunc(43)
		runtime.GC()
		result <- workerlocality.LoadTLSValue() + workerlocality.CallTLSFunc()
	}()
	if got := <-result; got != 85 {
		t.Fatalf("worker TLS values = %d, want 85", got)
	}
}

func TestConcurrentWorkerOutput(t *testing.T) {
	const writers = 16
	results := make(chan workerCallbackResult, writers)
	for range writers {
		go func() {
			origin := schedulerProcID()
			_, err := fmt.Fprint(os.Stdout, ".")
			results <- workerCallbackResult{origin: origin, err: err}
		}()
	}

	workers := make(map[int]bool)
	for range writers {
		result := <-results
		if result.err != nil {
			t.Fatal(result.err)
		}
		workers[result.origin] = true
	}
	if len(workers) < 2 {
		t.Fatalf("output ran on %d scheduler worker, want at least 2", len(workers))
	}
	fmt.Fprintln(os.Stdout)
}

func TestWorkerHostCallbackRealms(t *testing.T) {
	const callbacks = 16
	ready := make(chan struct{}, callbacks)
	start := make(chan struct{})
	results := make(chan workerCallbackResult, callbacks)
	for range callbacks {
		go func() {
			origin := schedulerProcID()
			done := make(chan int, 1)
			callback := js.FuncOf(func(js.Value, []js.Value) any {
				done <- schedulerProcID()
				return nil
			})
			defer callback.Release()
			ready <- struct{}{}
			<-start
			if current := schedulerProcID(); current != origin {
				results <- workerCallbackResult{origin: origin, err: fmt.Errorf("goroutine changed worker before JavaScript callback: got %d, want %d", current, origin)}
				return
			}
			js.Global().Call("setTimeout", callback, 0)
			// The outer timeout covers the complete callback batch. Do not create
			// one uncancellable time.After timer per callback: successful selects
			// leave those timers queued and repeated tests can overwhelm the host
			// event loop several seconds later.
			callbackWorker := <-done
			results <- workerCallbackResult{origin: origin, callback: callbackWorker}
		}()
	}
	for range callbacks {
		<-ready
	}
	// Every callback is now retained only by its suspended goroutine and the
	// physical worker's syscall/js registry. Collect before JavaScript receives
	// any of them so the test covers both root paths without starving the host
	// event loop behind one collection per callback.
	runtime.GC()
	close(start)

	workers := make(map[int]bool)
	timeout := time.After(30 * time.Second)
	for range callbacks {
		select {
		case result := <-results:
			if result.err != nil {
				t.Fatal(result.err)
			}
			if result.callback != result.origin {
				t.Fatalf("callback worker = %d, want origin worker %d", result.callback, result.origin)
			}
			workers[result.origin] = true
		case <-timeout:
			t.Fatal("worker callbacks did not make progress")
		}
	}
	if callbacks > 1 && len(workers) < 2 {
		t.Fatalf("callbacks ran on %d scheduler worker, want at least 2", len(workers))
	}
}
