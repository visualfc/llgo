//go:build windows

package gotest

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"unsafe"
)

const nonNilFaultChildEnv = "LLGO_TEST_NON_NIL_FAULT"

// Regression probe matching GOROOT/test/fixedbugs/issue73748b.go.
const windowsTraceFaultProbe = `package main

import (
	"context"
	"io"
	"runtime/trace"
)

type T struct { a [16]int }

//go:noinline
func f(x, y *T) { *x = *y }

func main() {
	trace.Start(io.Discard)
	defer func() {
		recover()
		trace.Log(context.Background(), "a", "b")
	}()
	f(nil, nil)
}
`

func TestRecoverFaultWhileTracing(t *testing.T) {
	compiler := faultLLGo(t)
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module tracefault\n\ngo 1.21\n"), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "main.go"), []byte(windowsTraceFaultProbe), 0644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command(compiler, "run", ".")
	cmd.Dir = dir
	if output, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("recovering a fault while tracing failed: %v\n%s", err, output)
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
