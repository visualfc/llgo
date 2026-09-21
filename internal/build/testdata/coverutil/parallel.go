package coverutil

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

// WaitForPeer makes serialized test execution fail deterministically. A shared
// -coverprofile must not stop two test binaries from occupying separate workers.
func WaitForPeer(t *testing.T, name, peer string) {
	t.Helper()
	dir := os.Getenv("LLGO_COVER_BARRIER")
	if dir == "" {
		return
	}
	if err := os.WriteFile(filepath.Join(dir, name), nil, 0600); err != nil {
		t.Fatal(err)
	}
	deadline := time.Now().Add(30 * time.Second)
	for {
		if _, err := os.Stat(filepath.Join(dir, peer)); err == nil {
			return
		} else if !os.IsNotExist(err) {
			t.Fatal(err)
		}
		if time.Now().After(deadline) {
			t.Fatal("coverage test binaries ran sequentially")
		}
		time.Sleep(time.Millisecond)
	}
}
