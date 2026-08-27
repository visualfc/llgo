//go:build windows

package windowstesting

import (
	"fmt"
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestBasic(t *testing.T) {
	if testing.Short() {
		t.Fatal("basic Windows testing smoke unexpectedly ran in short mode")
	}
}

func TestSubtestAndCleanup(t *testing.T) {
	cleaned := false
	if !t.Run("child", func(t *testing.T) {
		t.Cleanup(func() { cleaned = true })
	}) {
		t.Fatal("passing child subtest reported failure")
	}
	if !cleaned {
		t.Fatal("child cleanup did not run before t.Run returned")
	}
}

func TestTempDirAndEnvironment(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "result.txt")
	if err := os.WriteFile(path, []byte("ok"), 0o644); err != nil {
		t.Fatal(err)
	}
	if data, err := os.ReadFile(path); err != nil || string(data) != "ok" {
		t.Fatalf("temporary file = %q, %v", data, err)
	}

	const key = "LLGO_WINDOWS_TESTING_ENV"
	t.Setenv(key, "ok")
	if got := os.Getenv(key); got != "ok" {
		t.Fatalf("environment value = %q, want ok", got)
	}
}

func TestDeadline(t *testing.T) {
	deadline, ok := t.Deadline()
	if !ok {
		t.Fatal("test binary has no deadline")
	}
	if remaining := time.Until(deadline); remaining <= 0 {
		t.Fatalf("test deadline already expired: %v", remaining)
	}
}

func TestParallelOne(t *testing.T) {
	t.Parallel()
	time.Sleep(10 * time.Millisecond)
}

func TestParallelTwo(t *testing.T) {
	t.Parallel()
	time.Sleep(10 * time.Millisecond)
}

func TestExpectedFailure(t *testing.T) {
	if os.Getenv("LLGO_WINDOWS_TEST_EXPECT_FAILURE") == "1" {
		t.Fatal("intentional Windows test failure")
	}
}

func BenchmarkTestingSmoke(b *testing.B) {
	for i := 0; i < b.N; i++ {
	}
}

func Example() {
	fmt.Println("windows testing example")
	// Output:
	// windows testing example
}
