//go:build windows

package windowstesting

import (
	"sync/atomic"
	"testing"
)

func TestAtomicValueRuntimeHooks(t *testing.T) {
	var value atomic.Value
	value.Store("first")
	if !value.CompareAndSwap("first", "second") {
		t.Fatal("CompareAndSwap did not replace the stored value")
	}
	if old := value.Swap("third"); old != "second" {
		t.Fatalf("Swap returned %v, want second", old)
	}
	if got := value.Load(); got != "third" {
		t.Fatalf("Load returned %v, want third", got)
	}
}
