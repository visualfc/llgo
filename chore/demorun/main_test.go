package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/internal/demotest"
)

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
