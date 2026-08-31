//go:build llgo && !baremetal && !nogc && !wasm

package finalizerstress

import (
	"os"
	"runtime"
	"sync"
	"testing"
	"time"
	_ "unsafe"
)

type finalizerValue struct {
	value int
	pad   [64]byte
}

//go:linkname getBDWGCFinalizeOnDemand C.GC_get_finalize_on_demand
func getBDWGCFinalizeOnDemand() int32

//go:linkname setBDWGCFinalizeOnDemand C.GC_set_finalize_on_demand
func setBDWGCFinalizeOnDemand(enabled int32)

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

func TestConcurrentGCFinalizerQueue(t *testing.T) {
	// Keep finalization inside the concurrent runtime.GC calls below instead
	// of letting an unrelated allocation drain BDWGC's ready queue first.
	old := getBDWGCFinalizeOnDemand()
	setBDWGCFinalizeOnDemand(1)
	t.Cleanup(func() {
		setBDWGCFinalizeOnDemand(old)
	})

	rounds := stressCount(t, 8)
	objects := stressCount(t, 2048)
	workers := stressCount(t, 32)
	for round := 0; round < rounds; round++ {
		runConcurrentGCFinalizerRound(t, round, objects, workers)
	}
}

func runConcurrentGCFinalizerRound(t *testing.T, round, objects, workers int) {
	t.Helper()
	finalized := make(chan int, objects)
	registered := make(chan struct{})
	go func() {
		for i := 0; i < objects; i++ {
			p := &finalizerValue{value: i}
			runtime.SetFinalizer(p, func(p *finalizerValue) {
				finalized <- p.value
			})
		}
		close(registered)
	}()
	<-registered

	seen := make(map[int]bool, objects)
	deadline := time.After(30 * time.Second)
	for len(seen) <= objects/2 {
		var work sync.WaitGroup
		work.Add(workers)
		for i := 0; i < workers; i++ {
			go func() {
				defer work.Done()
				runtime.GC()
			}()
		}
		work.Wait()

		for {
			select {
			case value := <-finalized:
				if value < 0 || value >= objects {
					t.Fatalf("round %d: finalizer got %d, want [0,%d)", round, value, objects)
				}
				if seen[value] {
					t.Fatalf("round %d: finalizer got duplicate value %d", round, value)
				}
				seen[value] = true
			default:
				goto drained
			}
		}
	drained:
		if len(seen) > objects/2 {
			return
		}
		runtime.Gosched()
		select {
		case <-deadline:
			t.Fatalf("round %d: only %d/%d concurrent finalizers ran", round, len(seen), objects)
		default:
		}
	}
}
