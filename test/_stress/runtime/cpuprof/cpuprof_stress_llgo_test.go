//go:build llgo && !baremetal && !wasm && (darwin || linux) && (amd64 || arm64)

package cpuprofstress

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"runtime/pprof"
	"sync"
	"sync/atomic"
	"syscall"
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

const modeChurnHelperEnv = "LLGO_STRESS_SIGPROF_MODE_CHURN"

func TestSIGPROFModeChurnWhileProfiling(t *testing.T) {
	if os.Getenv(modeChurnHelperEnv) != "1" {
		cmd := exec.Command(os.Args[0], "-test.run=^TestSIGPROFModeChurnWhileProfiling$", "-test.v")
		cmd.Env = append(os.Environ(), modeChurnHelperEnv+"=1")
		if output, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("SIGPROF mode-churn child failed: %v\n%s", err, output)
		}
		return
	}

	notified := make(chan os.Signal, 1)
	signal.Notify(notified, syscall.SIGPROF)
	t.Cleanup(func() {
		signal.Stop(notified)
		signal.Reset(syscall.SIGPROF)
	})
	raisers := stressCount(t, 8)
	rounds := stressCount(t, 2000)
	pid := syscall.Getpid()
	stop := make(chan struct{})
	var failures atomic.Int64
	var work sync.WaitGroup
	work.Add(raisers)
	for worker := 0; worker < raisers; worker++ {
		go func() {
			defer work.Done()
			for {
				select {
				case <-stop:
					return
				default:
					if syscall.Kill(pid, syscall.SIGPROF) != nil {
						failures.Add(1)
						return
					}
				}
			}
		}()
	}
	done := make(chan struct{})
	go func() {
		work.Wait()
		close(done)
	}()
	stopped := false
	stopRaisers := func() bool {
		if !stopped {
			close(stop)
			stopped = true
		}
		select {
		case <-done:
			return true
		case <-time.After(30 * time.Second):
			return false
		}
	}

	select {
	case got := <-notified:
		if got != syscall.SIGPROF {
			if !stopRaisers() {
				t.Fatal("SIGPROF raisers did not stop within 30s")
			}
			t.Fatalf("got initial signal %v, want SIGPROF", got)
		}
	case <-time.After(30 * time.Second):
		if !stopRaisers() {
			t.Fatal("SIGPROF raisers did not stop within 30s")
		}
		t.Fatal("signal flood did not deliver an initial SIGPROF")
	}

	var profile bytes.Buffer
	if err := pprof.StartCPUProfile(&profile); err != nil {
		if !stopRaisers() {
			t.Fatal("SIGPROF raisers did not stop within 30s")
		}
		t.Fatalf("StartCPUProfile: %v", err)
	}
	t.Cleanup(pprof.StopCPUProfile)
	t.Cleanup(func() {
		if !stopRaisers() {
			t.Error("SIGPROF raisers did not stop within 30s")
		}
	})

	var stateFailure string
	for round := 0; round < rounds; round++ {
		signal.Notify(notified, syscall.SIGPROF)
		signal.Stop(notified)
		signal.Ignore(syscall.SIGPROF)
		if !signal.Ignored(syscall.SIGPROF) {
			stateFailure = fmt.Sprintf("round %d: SIGPROF is not ignored", round)
			break
		}
		signal.Reset(syscall.SIGPROF)
		if signal.Ignored(syscall.SIGPROF) {
			stateFailure = fmt.Sprintf("round %d: SIGPROF is still ignored after Reset", round)
			break
		}
	}
	if !stopRaisers() {
		t.Fatal("SIGPROF raisers did not stop within 30s")
	}
	if stateFailure != "" {
		t.Fatal(stateFailure)
	}
	if got := failures.Load(); got != 0 {
		t.Fatalf("kill(SIGPROF) failed in %d workers", got)
	}

	pprof.StopCPUProfile()
	if profile.Len() == 0 {
		t.Fatal("CPU profile is empty")
	}
}
