package main

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/internal/demotest"
)

func TestDefaultJobsIsBounded(t *testing.T) {
	if got := defaultJobs(); got < 1 || got > 4 || got > runtime.NumCPU() {
		t.Fatalf("defaultJobs() = %d, NumCPU = %d", got, runtime.NumCPU())
	}
}

func TestAppendResultUsesManifestOrder(t *testing.T) {
	path := filepath.Join(t.TempDir(), "result.md")
	report := demotest.Report{
		Profile: "host",
		Results: []demotest.CaseResult{
			{Case: demotest.PlannedCase{Case: demotest.Case{Dir: "_demo/go/first"}}, Err: os.ErrInvalid},
			{Case: demotest.PlannedCase{Case: demotest.Case{Dir: "_demo/go/pass"}}},
			{Case: demotest.PlannedCase{Case: demotest.Case{Dir: "_demo/go/second"}}, Err: os.ErrPermission},
		},
	}
	if err := appendResult(path, report); err != nil {
		t.Fatal(err)
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	got := string(data)
	first := strings.Index(got, "_demo/go/first")
	second := strings.Index(got, "_demo/go/second")
	if first < 0 || second <= first {
		t.Fatalf("failures are not in manifest order:\n%s", got)
	}
}

func TestAppendResultAggregatesProfiles(t *testing.T) {
	path := filepath.Join(t.TempDir(), "result.md")
	for _, profile := range []string{"esp32", "esp32c3-basic"} {
		if err := appendResult(path, demotest.Report{Profile: profile}); err != nil {
			t.Fatal(err)
		}
	}
	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	want := ":white_check_mark: All demo tests passed (esp32)\n" +
		":white_check_mark: All demo tests passed (esp32c3-basic)\n"
	if got := string(data); got != want {
		t.Fatalf("result.md = %q, want %q", got, want)
	}
}
