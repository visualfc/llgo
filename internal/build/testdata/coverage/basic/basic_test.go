package basic

import (
	"testing"

	"github.com/xgo-dev/llgo/internal/build/testdata/coverutil"
)

func TestBranch(t *testing.T) {
	coverutil.WaitForPeer(t)
	if Initial != 1 || Branch(true) != 1 {
		t.Fatal("wrong branch")
	}
	if testing.CoverMode() != "" && testing.Coverage() == 0 {
		t.Fatal("missing coverage counters")
	}
}
