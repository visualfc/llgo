package other_test

import (
	"testing"

	"github.com/xgo-dev/llgo/internal/build/testdata/coverage/other"
	"github.com/xgo-dev/llgo/internal/build/testdata/coverutil"
)

func TestValue(t *testing.T) {
	coverutil.WaitForPeer(t, "other", "basic")
	if other.Value() != 7 {
		t.Fatal("wrong value")
	}
}
