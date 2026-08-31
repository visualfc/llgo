//go:build llgo && !baremetal && !wasm && !windows && !plan9

package signalstress

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"sync"
	"sync/atomic"
	"syscall"
	"testing"
	"time"
	_ "unsafe"
)

//go:linkname cRaise C.raise
func cRaise(sig int32) int32

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

func TestConcurrentRaisePreservesDistinctSignal(t *testing.T) {
	workers := stressCount(t, 512)
	perWorker := stressCount(t, 2000)
	rareRounds := stressCount(t, 64)
	floodSignal := syscall.SIGCHLD
	distinctSignal := syscall.SIGWINCH
	if floodSignal >= distinctSignal {
		t.Skipf("need a harmless flood signal below %v; got %v", distinctSignal, floodSignal)
	}

	// Flood a lower-numbered signal so a receiver that repeatedly starts its
	// global scan at signal 1 can starve the distinct higher-numbered signal.
	flooded := make(chan os.Signal, 1)
	distinct := make(chan os.Signal, 1)
	signal.Notify(flooded, floodSignal)
	signal.Notify(distinct, distinctSignal)

	start := make(chan struct{})
	stop := make(chan struct{})
	var failures atomic.Int64
	var ready sync.WaitGroup
	var minimum sync.WaitGroup
	var work sync.WaitGroup
	ready.Add(workers)
	minimum.Add(workers)
	work.Add(workers)
	for i := 0; i < workers; i++ {
		go func() {
			defer work.Done()
			<-start
			ready.Done()
			minimumReported := false
			defer func() {
				if !minimumReported {
					minimum.Done()
				}
			}()
			for n := 0; n < perWorker; n++ {
				select {
				case <-stop:
					return
				default:
				}
				if cRaise(int32(floodSignal)) != 0 {
					failures.Add(1)
					return
				}
			}
			minimum.Done()
			minimumReported = true
			for {
				select {
				case <-stop:
					return
				default:
					if cRaise(int32(floodSignal)) != 0 {
						failures.Add(1)
						return
					}
				}
			}
		}()
	}
	close(start)

	var distinctFailure error
	if err := waitGroup(&ready, 30*time.Second); err != nil {
		distinctFailure = fmt.Errorf("signal flood did not become ready: %w", err)
	} else {
		for round := 0; round < rareRounds; round++ {
			if cRaise(int32(distinctSignal)) != 0 {
				distinctFailure = fmt.Errorf("round %d: raise(%v) failed", round, distinctSignal)
				break
			}
			select {
			case got := <-distinct:
				if got != distinctSignal {
					distinctFailure = fmt.Errorf("round %d: got %v, want %v", round, got, distinctSignal)
				}
			case <-time.After(10 * time.Second):
				distinctFailure = fmt.Errorf("round %d: distinct signal starved during flood", round)
			}
			if distinctFailure != nil {
				break
			}
		}
	}
	var minimumErr error
	if distinctFailure == nil {
		minimumErr = waitGroup(&minimum, 5*time.Minute)
	}
	close(stop)
	workErr := waitGroup(&work, 30*time.Second)
	signal.Stop(distinct)
	signal.Stop(flooded)

	if workErr != nil {
		t.Fatal(workErr)
	}
	if minimumErr != nil {
		t.Fatal(minimumErr)
	}
	if got := failures.Load(); got != 0 {
		t.Fatalf("raise(%v) failed in %d workers", floodSignal, got)
	}
	if distinctFailure != nil {
		t.Fatal(distinctFailure)
	}
	select {
	case <-flooded:
	default:
		t.Fatal("signal flood delivered no notification")
	}
}

func TestNotifyStopResetBarrier(t *testing.T) {
	rounds := stressCount(t, 2000)
	for round := 0; round < rounds; round++ {
		c := make(chan os.Signal, 1)
		signal.Notify(c, syscall.SIGWINCH)
		if cRaise(int32(syscall.SIGWINCH)) != 0 {
			t.Fatalf("round %d: raise failed", round)
		}
		signal.Stop(c)
		select {
		case <-c:
		default:
			t.Fatalf("round %d: signal sent before Stop was lost", round)
		}
		signal.Reset(syscall.SIGWINCH)
	}
}

const atomicStopStressHelperEnv = "LLGO_STRESS_ATOMIC_SIGNAL_STOP"

func TestAtomicStopRace(t *testing.T) {
	if os.Getenv(atomicStopStressHelperEnv) == "1" {
		runAtomicStopStressHelper(t, stressCount(t, 128))
		return
	}

	// Keep SIGINT from being inherited as ignored by the helper. Each raced
	// signal must either reach the stopping channel or take the default action.
	parentSignals := make(chan os.Signal, 1)
	signal.Notify(parentSignals, syscall.SIGINT)
	defer signal.Stop(parentSignals)

	helpers := stressCount(t, 64)
	results := make(chan atomicStopResult, helpers)
	for helper := 0; helper < helpers; helper++ {
		go func(helper int) {
			cmd := exec.Command(os.Args[0], "-test.run=^TestAtomicStopRace$")
			cmd.Env = append(os.Environ(), atomicStopStressHelperEnv+"=1")
			out, err := cmd.CombinedOutput()
			results <- atomicStopResult{helper, out, err}
		}(helper)
	}
	for received := 0; received < helpers; received++ {
		result := <-results
		if bytes.Contains(result.out, []byte("lost signal")) {
			t.Errorf("helper %d dropped a signal during Stop:\n%s",
				result.helper, result.out)
			continue
		}
		if result.err == nil {
			continue
		}
		exitErr, ok := result.err.(*exec.ExitError)
		if !ok {
			t.Errorf("helper %d: %v", result.helper, result.err)
			continue
		}
		status, ok := exitErr.Sys().(syscall.WaitStatus)
		if !ok || !status.Signaled() || status.Signal() != syscall.SIGINT {
			t.Errorf("helper %d exited unexpectedly: %v\n%s",
				result.helper, result.err, result.out)
		}
	}
}

type atomicStopResult struct {
	helper int
	out    []byte
	err    error
}

func runAtomicStopStressHelper(t *testing.T, tries int) {
	t.Helper()
	if signal.Ignored(syscall.SIGINT) {
		fmt.Println("SIGINT is ignored")
		os.Exit(2)
	}

	pid := syscall.Getpid()
	for try := 0; try < tries; try++ {
		c := make(chan os.Signal, 1)
		signal.Notify(c, syscall.SIGINT)

		var stopped sync.WaitGroup
		stopped.Add(1)
		go func() {
			defer stopped.Done()
			signal.Stop(c)
		}()

		if err := syscall.Kill(pid, syscall.SIGINT); err != nil {
			fmt.Printf("kill: %v\n", err)
			os.Exit(2)
		}
		select {
		case <-c:
		case <-time.After(2 * time.Second):
			fmt.Printf("lost signal on try %d\n", try)
			os.Exit(3)
		}
		stopped.Wait()
	}
	os.Exit(0)
}

func TestConcurrentNotifyStopDuringSignalFlood(t *testing.T) {
	raisers := stressCount(t, 64)
	churners := stressCount(t, 32)
	roundsPerChurner := stressCount(t, 64)
	sentinel := make(chan os.Signal, 1)
	signal.Notify(sentinel, syscall.SIGWINCH)

	raiserStart := make(chan struct{})
	churnStart := make(chan struct{})
	abortChurn := make(chan struct{})
	stop := make(chan struct{})
	var raiseFailures atomic.Int64
	var lost atomic.Int64
	var ready sync.WaitGroup
	var raiserWork sync.WaitGroup
	var churnWork sync.WaitGroup
	ready.Add(raisers)
	raiserWork.Add(raisers)
	churnWork.Add(churners)
	for i := 0; i < raisers; i++ {
		go func() {
			defer raiserWork.Done()
			<-raiserStart
			ready.Done()
			for {
				select {
				case <-stop:
					return
				default:
					if cRaise(int32(syscall.SIGWINCH)) != 0 {
						raiseFailures.Add(1)
						return
					}
				}
			}
		}()
	}
	for i := 0; i < churners; i++ {
		go func() {
			defer churnWork.Done()
			<-churnStart
			for round := 0; round < roundsPerChurner; round++ {
				select {
				case <-abortChurn:
					return
				default:
				}
				c := make(chan os.Signal, 1)
				signal.Notify(c, syscall.SIGWINCH)
				if cRaise(int32(syscall.SIGWINCH)) != 0 {
					raiseFailures.Add(1)
					signal.Stop(c)
					return
				}
				signal.Stop(c)
				select {
				case <-c:
				default:
					lost.Add(1)
				}
			}
		}()
	}
	close(raiserStart)
	readyErr := waitGroup(&ready, 30*time.Second)
	close(churnStart)
	churnErr := waitGroup(&churnWork, 5*time.Minute)
	if churnErr != nil {
		close(abortChurn)
		_ = waitGroup(&churnWork, 30*time.Second)
	}
	close(stop)
	raiserErr := waitGroup(&raiserWork, 30*time.Second)
	signal.Stop(sentinel)

	if readyErr != nil {
		t.Fatal(readyErr)
	}
	if churnErr != nil {
		t.Fatal(churnErr)
	}
	if raiserErr != nil {
		t.Fatal(raiserErr)
	}
	if got := raiseFailures.Load(); got != 0 {
		t.Fatalf("raise(SIGWINCH) failed %d times", got)
	}
	if got := lost.Load(); got != 0 {
		t.Fatalf("concurrent Notify/Stop lost %d pre-Stop signals", got)
	}
	select {
	case <-sentinel:
	default:
		t.Fatal("signal flood delivered no sentinel notification")
	}
}

func TestTimerProgressDuringSignalFlood(t *testing.T) {
	workers := stressCount(t, 64)
	notified := make(chan os.Signal, 1)
	signal.Notify(notified, syscall.SIGWINCH)

	start := make(chan struct{})
	stop := make(chan struct{})
	var failures atomic.Int64
	var raises atomic.Int64
	var ready sync.WaitGroup
	var work sync.WaitGroup
	ready.Add(workers)
	work.Add(workers)
	for i := 0; i < workers; i++ {
		go func() {
			defer work.Done()
			<-start
			if cRaise(int32(syscall.SIGWINCH)) != 0 {
				failures.Add(1)
				ready.Done()
				return
			}
			raises.Add(1)
			ready.Done()
			for {
				select {
				case <-stop:
					return
				default:
					if cRaise(int32(syscall.SIGWINCH)) != 0 {
						failures.Add(1)
						return
					}
					raises.Add(1)
				}
			}
		}()
	}
	close(start)
	readyErr := waitGroup(&ready, 30*time.Second)
	timer := time.NewTimer(20 * time.Millisecond)
	deadline := time.NewTimer(10 * time.Second)
	var timerErr error
	if readyErr == nil {
		select {
		case <-timer.C:
		case <-deadline.C:
			timerErr = fmt.Errorf("runtime timer made no progress during signal flood")
		}
	}
	timer.Stop()
	deadline.Stop()
	close(stop)
	workErr := waitGroup(&work, 30*time.Second)
	signal.Stop(notified)

	if readyErr != nil {
		t.Fatal(readyErr)
	}
	if timerErr != nil {
		t.Fatal(timerErr)
	}
	if workErr != nil {
		t.Fatal(workErr)
	}
	if got := failures.Load(); got != 0 {
		t.Fatalf("raise(SIGWINCH) failed in %d workers", got)
	}
	if got := raises.Load(); got < int64(workers) {
		t.Fatalf("signal flood raised %d signals, want at least %d", got, workers)
	}
}
