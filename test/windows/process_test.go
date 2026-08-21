//go:build windows

package windowstesting

import (
	"os/exec"
	"testing"
)

func TestRepeatedProcessLifecycle(t *testing.T) {
	const iterations = 32
	for i := 0; i < iterations; i++ {
		cmd := exec.Command(`C:\Windows\System32\cmd.exe`, "/c", "exit", "0")
		if err := cmd.Run(); err != nil {
			t.Fatalf("iteration %d: %v", i, err)
		}
	}
}
