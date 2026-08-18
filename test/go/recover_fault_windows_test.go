//go:build windows

package gotest

import (
	"os"
	"os/exec"
	"testing"
	"unsafe"
)

const nonNilFaultChildEnv = "LLGO_TEST_NON_NIL_FAULT"

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
		page, _ := protectedMemory(t, 1, 0, 1)
		if page[0] != 0 {
			t.Fatal("unexpected protected-page value")
		}
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
