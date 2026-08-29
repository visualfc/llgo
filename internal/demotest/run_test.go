package demotest

import (
	"bytes"
	"context"
	"errors"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"
)

func TestRunUsesLockedGoEnvironmentAndStableOutputOrder(t *testing.T) {
	t.Setenv("GOFLAGS", "")
	manifest := &Manifest{
		Profiles: []Profile{{Name: "host", LLGOArgs: []string{"-flag"}}},
		Cases: []Case{
			{ID: "first", Dir: "_demo/go/first", Profiles: []string{"host"}, GOOS: []string{"linux"}, Check: Check{Kind: "self"}},
			{ID: "second", Dir: "_demo/go/second", Profiles: []string{"host"}, GOOS: []string{"linux"}, Check: Check{Kind: "self"}},
		},
	}
	var mu sync.Mutex
	var calls []string
	command := func(_ context.Context, dir, executable string, args, env []string) ([]byte, []byte, error) {
		if strings.HasSuffix(dir, "first") {
			time.Sleep(10 * time.Millisecond)
		}
		mu.Lock()
		calls = append(calls, strings.Join([]string{
			filepath.Base(dir), executable, strings.Join(args, " "), envValue(env, "GOTOOLCHAIN"), envValue(env, "GOWORK"), envValue(env, "GOFLAGS"),
		}, "|"))
		mu.Unlock()
		return []byte(filepath.Base(dir) + "\n"), nil, nil
	}
	var output bytes.Buffer
	report, err := Run(context.Background(), manifest, RunOptions{
		Root: "/repo", Profile: "host", GOOS: "linux", Jobs: 2, LLGO: "test-llgo", Out: &output, Command: command,
	})
	if err != nil {
		t.Fatal(err)
	}
	if report.Failed() != 0 || len(calls) != 2 {
		t.Fatalf("report = %#v, calls = %q", report, calls)
	}
	for _, call := range calls {
		parts := strings.Split(call, "|")
		if !reflect.DeepEqual(parts[1:], []string{"test-llgo", "run -flag .", "local", "off", "-mod=readonly"}) {
			t.Fatalf("command = %q", call)
		}
	}
	got := output.String()
	if strings.Index(got, "Testing _demo/go/first") > strings.Index(got, "Testing _demo/go/second") {
		t.Fatalf("output is not in manifest order:\n%s", got)
	}
	assertOrdered(t, got,
		"first\nPASS _demo/go/first\n",
		"second\nPASS _demo/go/second\n",
	)
}

func TestRunSequentialOutputKeepsEachCaseTogether(t *testing.T) {
	t.Setenv("GOFLAGS", "")
	manifest := &Manifest{
		Profiles: []Profile{{Name: "host"}},
		Cases: []Case{
			{ID: "first", Dir: "_demo/go/first", Profiles: []string{"host"}, GOOS: []string{"linux"}, Check: Check{Kind: "self"}},
			{ID: "second", Dir: "_demo/go/second", Profiles: []string{"host"}, GOOS: []string{"linux"}, Check: Check{Kind: "self"}},
		},
	}
	command := func(_ context.Context, dir, _ string, _ []string, _ []string) ([]byte, []byte, error) {
		if strings.HasSuffix(dir, "second") {
			return nil, []byte("second stderr\n"), errors.New("second failed")
		}
		return []byte("first stdout\n"), nil, nil
	}
	var output bytes.Buffer
	report, err := Run(context.Background(), manifest, RunOptions{
		Root: "/repo", Profile: "host", GOOS: "linux", Jobs: 1, Out: &output, Command: command,
	})
	if err != nil {
		t.Fatal(err)
	}
	if report.Failed() != 1 {
		t.Fatalf("report = %#v", report)
	}
	assertOrdered(t, output.String(),
		"Testing _demo/go/first\n",
		"first stdout\n",
		"PASS _demo/go/first\n",
		"Testing _demo/go/second\n",
		"second stderr\n",
		"FAIL _demo/go/second: expected successful exit: second failed\n",
	)
}

func TestRunPrintsCaseBeforeExecuting(t *testing.T) {
	t.Setenv("GOFLAGS", "")
	manifest := &Manifest{
		Profiles: []Profile{{Name: "host"}},
		Cases: []Case{{
			ID: "visible", Dir: "_demo/go/visible", Profiles: []string{"host"}, GOOS: []string{"linux"}, Check: Check{Kind: "self"},
		}},
	}
	var output bytes.Buffer
	visibleAtRun := false
	command := func(_ context.Context, _ string, _ string, _ []string, _ []string) ([]byte, []byte, error) {
		visibleAtRun = strings.Contains(output.String(), "Testing _demo/go/visible")
		return nil, nil, nil
	}
	if _, err := Run(context.Background(), manifest, RunOptions{
		Root: "/repo", Profile: "host", GOOS: "linux", Jobs: 1, Out: &output, Command: command,
	}); err != nil {
		t.Fatal(err)
	}
	if !visibleAtRun {
		t.Fatal("case start was not printed before command execution")
	}
}

func TestCommandEnvironmentPreservesFlagsAndRejectsModOverride(t *testing.T) {
	t.Setenv("GOFLAGS", "-tags=dev")
	env, err := commandEnvironment(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	if got, want := envValue(env, "GOFLAGS"), "-tags=dev -mod=readonly"; got != want {
		t.Fatalf("GOFLAGS = %q, want %q", got, want)
	}

	t.Setenv("GOFLAGS", "-mod=readonly -tags=dev")
	env, err = commandEnvironment(t.TempDir())
	if err != nil {
		t.Fatal(err)
	}
	if got, want := envValue(env, "GOFLAGS"), "-mod=readonly -tags=dev"; got != want {
		t.Fatalf("GOFLAGS = %q, want %q", got, want)
	}

	t.Setenv("GOFLAGS", "-mod=vendor")
	if _, err := commandEnvironment(t.TempDir()); err == nil {
		t.Fatal("commandEnvironment unexpectedly accepted -mod=vendor")
	}
}

func TestRunNeverTreatsTimeoutAsExpectedFailure(t *testing.T) {
	t.Setenv("GOFLAGS", "")
	manifest := &Manifest{
		Profiles: []Profile{{Name: "host"}},
		Cases: []Case{{
			ID: "timeout", Dir: "_demo/go/timeout", Profiles: []string{"host"}, GOOS: []string{"linux"},
			Timeout: "1ms", Check: Check{Kind: "failure", StderrContains: []string{"expected fragment"}},
		}},
	}
	command := func(ctx context.Context, _ string, _ string, _ []string, _ []string) ([]byte, []byte, error) {
		<-ctx.Done()
		return nil, []byte("expected fragment"), ctx.Err()
	}
	report, err := Run(context.Background(), manifest, RunOptions{
		Root: "/repo", Profile: "host", GOOS: "linux", Jobs: 1, Command: command,
	})
	if err != nil {
		t.Fatal(err)
	}
	if report.Failed() != 1 || !strings.Contains(report.Results[0].Err.Error(), "timed out") {
		t.Fatalf("timeout report = %#v", report)
	}
}

func envValue(env []string, key string) string {
	prefix := key + "="
	for _, value := range env {
		if strings.HasPrefix(value, prefix) {
			return strings.TrimPrefix(value, prefix)
		}
	}
	return ""
}

func assertOrdered(t *testing.T, output string, fragments ...string) {
	t.Helper()
	remaining := output
	for _, fragment := range fragments {
		index := strings.Index(remaining, fragment)
		if index < 0 {
			t.Fatalf("output does not contain %q in order:\n%s", fragment, output)
		}
		remaining = remaining[index+len(fragment):]
	}
}
