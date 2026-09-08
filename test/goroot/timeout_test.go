package goroot

import (
	"fmt"
	"os"
	"os/exec"
	"strings"
	"testing"
	"time"
)

// guardTestTimeout bounds one runner unit test, including its subtests and
// cleanup. The external GOROOT/test suite has separate build/run budgets and
// must not use this unit-test limit. A runtime-wide timer failure still needs
// an external process watchdog; this is not a substitute for that watchdog.
func guardTestTimeout(t *testing.T) {
	t.Helper()
	armTestTimeout(t, 3*time.Minute)
	// Helper subprocesses have their own output contracts (including empty
	// output in the termination tests). Log their parent case, not the helper.
	if os.Getenv("LLGO_GOROOT_HELPER") != "" || os.Getenv("LLGO_GOROOT_TIMEOUT_HELPER") != "" {
		return
	}
	name, start := t.Name(), time.Now()
	fmt.Fprintf(os.Stderr, "goroot case START %s (timeout 3m)\n", name)
	t.Cleanup(func() {
		fmt.Fprintf(os.Stderr, "goroot case END %s (%s)\n", name, time.Since(start).Round(time.Millisecond))
	})
}

func armTestTimeout(t *testing.T, timeout time.Duration) *time.Timer {
	t.Helper()
	name := t.Name()
	timer := time.AfterFunc(timeout, func() {
		// Fatal cannot stop the test from another goroutine. Panic deliberately
		// fails the process, retaining the case name even without verbose output.
		panic(fmt.Sprintf("goroot unit test %s timed out after %s", name, timeout))
	})
	// Register first so later test cleanups also run within the timeout.
	t.Cleanup(func() { timer.Stop() })
	return timer
}

func TestCaseTimeout(t *testing.T) {
	guardTestTimeout(t)
	switch os.Getenv("LLGO_GOROOT_TIMEOUT_HELPER") {
	case "expire":
		armTestTimeout(t, 50*time.Millisecond)
		select {}
	case "cleanup":
		armTestTimeout(t, 50*time.Millisecond)
		t.Cleanup(func() { select {} })
		return
	case "cancel":
		var timer *time.Timer
		t.Run("finished", func(t *testing.T) {
			timer = armTestTimeout(t, 3*time.Minute)
		})
		if timer.Stop() {
			t.Fatal("test cleanup did not stop its timer")
		}
		return
	case "wait":
		cmd := exec.Command("blocked-helper")
		cmd.Process = &os.Process{Pid: 123}
		waitTerminatedProgram(cmd, make(chan error), 50*time.Millisecond)
		return
	}
	disableSystemMemoryLimits(t)
	for _, mode := range []string{"expire", "cleanup", "cancel", "wait"} {
		t.Run(mode, func(t *testing.T) {
			env := upsertEnv(os.Environ(), "LLGO_GOROOT_TIMEOUT_HELPER="+mode)
			stdout, stderr, code, _, err := runProgram(t.TempDir(), os.Args[0], env, 10*time.Second,
				"-test.run=^TestCaseTimeout$", "-test.count=1", "-test.timeout=5s")
			if err != nil {
				t.Fatalf("timeout helper: %v\n%s\n%s", err, stdout, stderr)
			}
			if mode == "cancel" {
				if code != 0 {
					t.Fatalf("completed test retained its timer: exit %d\n%s\n%s", code, stdout, stderr)
				}
				return
			}
			want := "goroot unit test TestCaseTimeout timed out after 50ms"
			if mode == "wait" {
				want = `goroot command ["blocked-helper"] (pid 123) did not finish within 50ms after termination`
			}
			if code == 0 || !strings.Contains(string(stderr), want) {
				t.Fatalf("missing case timeout failure: exit %d\n%s\n%s", code, stdout, stderr)
			}
		})
	}
}
