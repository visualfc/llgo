//go:build llgo && windows && (amd64 || arm64)

package pprof_test

import (
	"testing"
	_ "unsafe"
)

//go:linkname testCPUProfileWindowsFaultRecovery C.llgo_cpu_profile_test_fault_recovery
func testCPUProfileWindowsFaultRecovery() int32

func TestCPUProfileWindowsFaultRecovery(t *testing.T) {
	if got := testCPUProfileWindowsFaultRecovery(); got != 1 {
		t.Fatalf("guarded frame walk returned %d frames, want interrupted PC only", got)
	}
}
