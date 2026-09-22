//go:build llgo

package coverageprobe

import "testing"

func TestCoverageComesFromLLGo(t *testing.T) {
	if got := CoveredByLLGo(); got != 42 {
		t.Fatalf("CoveredByLLGo() = %d, want 42", got)
	}
}
