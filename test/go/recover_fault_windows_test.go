//go:build windows

package gotest

import (
	"context"
	"io"
	"os"
	"os/exec"
	"runtime/trace"
	"testing"
	"unsafe"
)

const nonNilFaultChildEnv = "LLGO_TEST_NON_NIL_FAULT"

type traceFaultValue struct {
	a [16]int
}

//go:noinline
func copyTraceFaultValue(x, y *traceFaultValue) {
	*x = *y
}

// Regression test matching GOROOT/test/fixedbugs/issue73748b.go.
func TestRecoverFaultWhileTracing(t *testing.T) {
	if err := trace.Start(io.Discard); err != nil {
		t.Fatal(err)
	}
	defer trace.Stop()

	var recovered bool
	func() {
		defer func() {
			recovered = recover() != nil
			trace.Log(context.Background(), "a", "b")
		}()
		copyTraceFaultValue(nil, nil)
	}()
	if !recovered {
		t.Fatal("nil fault did not panic")
	}
}

func checkRecoveredFaultAddress(t *testing.T, err error, address *byte) {
	t.Helper()
	addressError, ok := err.(interface{ Addr() uintptr })
	if !ok {
		t.Fatalf("recovered fault %T does not report its address", err)
	}
	if got, want := addressError.Addr(), uintptr(unsafe.Pointer(address)); got != want {
		t.Fatalf("recovered fault address %#x, want %#x", got, want)
	}
}

func TestNonNilFaultRequiresPanicOnFault(t *testing.T) {
	if os.Getenv(nonNilFaultChildEnv) == "1" {
		func() {
			defer func() {
				_ = recover()
			}()
			page, _ := protectedMemory(t, 1, 0, 1)
			if page[0] != 0 {
				t.Fatal("unexpected protected-page value")
			}
		}()
		// A runtime that incorrectly turns the access violation into a Go
		// panic reaches here after recover and lets the child exit cleanly.
		// The parent requires Windows to terminate the process instead.
		return
	}

	cmd := exec.Command(os.Args[0], "-test.run=^TestNonNilFaultRequiresPanicOnFault$")
	cmd.Env = append(os.Environ(), nonNilFaultChildEnv+"=1")
	output, err := cmd.CombinedOutput()
	if err == nil {
		t.Fatalf("non-nil fault exited successfully:\n%s", output)
	}
	if _, ok := err.(*exec.ExitError); !ok {
		t.Fatalf("start non-nil fault child: %v", err)
	}
}
