package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/internal/demotest"
)

func TestMain(m *testing.M) {
	if mode := os.Getenv("LLGO_DEMORUN_HELPER"); mode != "" {
		fmt.Fprint(os.Stdout, "helper stdout")
		if mode == "failure" {
			os.Exit(9)
		}
		os.Exit(0)
	}
	os.Exit(m.Run())
}

func TestRunManifestAndListModes(t *testing.T) {
	root := writeDemoRepository(t)
	for _, test := range []struct {
		args []string
		want string
		code int
	}{
		{[]string{"-root", root, "-check-manifest"}, "manifest OK: 1 cases", 0},
		{[]string{"-root", root, "-list", "-case", "case"}, "case\t_demo/workflow/case\n1 cases", 0},
		{[]string{"-root", root, "-list", "-case", "missing"}, "not in the plan", 1},
		{[]string{"-jobs=bad"}, "invalid value", 2},
	} {
		var stdout, stderr bytes.Buffer
		if got := run(context.Background(), test.args, &stdout, &stderr); got != test.code {
			t.Fatalf("run(%q) = %d, want %d", test.args, got, test.code)
		}
		if output := stdout.String() + stderr.String(); !strings.Contains(output, test.want) {
			t.Fatalf("run(%q) output = %q, want %q", test.args, output, test.want)
		}
	}
}

func TestRunExitStatusAndResultSummary(t *testing.T) {
	t.Setenv("GOFLAGS", "")
	for _, test := range []struct {
		mode       string
		wantCode   int
		wantOutput string
		wantResult string
	}{
		{"success", 0, "1/1 tests passed", "All demo tests passed (host)"},
		{"failure", 1, "0/1 tests passed", "Failed demo cases (host):\n* :x: _demo/workflow/case"},
	} {
		t.Run(test.mode, func(t *testing.T) {
			t.Setenv("LLGO_DEMORUN_HELPER", test.mode)
			root := writeDemoRepository(t)
			var stdout, stderr bytes.Buffer
			args := []string{"-root", root, "-jobs", "1", "-llgo", os.Args[0], "-result", "result.md"}
			if got := run(context.Background(), args, &stdout, &stderr); got != test.wantCode {
				t.Fatalf("run = %d; stdout=%q stderr=%q", got, stdout.String(), stderr.String())
			}
			if !strings.Contains(stdout.String(), test.wantOutput) {
				t.Fatalf("stdout = %q, want %q", stdout.String(), test.wantOutput)
			}
			data, err := os.ReadFile(filepath.Join(root, "result.md"))
			if err != nil || !strings.Contains(string(data), test.wantResult) {
				t.Fatalf("result = %q, %v; want %q", data, err, test.wantResult)
			}
		})
	}
}

func writeDemoRepository(t *testing.T) string {
	t.Helper()
	root := t.TempDir()
	for _, dir := range []string{"_demo/go", "_demo/c", "_demo/py", "_demo/embed", "_demo/workflow/case"} {
		if err := os.MkdirAll(filepath.Join(root, filepath.FromSlash(dir)), 0o777); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.WriteFile(filepath.Join(root, "_demo", "workflow", "case", "main.go"), []byte("package main\n"), 0o666); err != nil {
		t.Fatal(err)
	}
	manifest := demotest.Manifest{
		Version:  demotest.ManifestVersion,
		Profiles: []demotest.Profile{{Name: "host"}},
		Cases: []demotest.Case{{
			ID: "case", Dir: "_demo/workflow/case", Profiles: []string{"host"}, GOOS: []string{runtime.GOOS},
		}},
		Support:  []demotest.SupportDirectory{},
		Workflow: []demotest.WorkflowDirectory{},
	}
	data, err := json.Marshal(manifest)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(root, "_demo", "manifest.json"), data, 0o666); err != nil {
		t.Fatal(err)
	}
	return root
}
