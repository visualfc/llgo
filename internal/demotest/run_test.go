package demotest

import (
	"bytes"
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"
)

func TestRunWorkerPoolSchedulesPastSlowCaseAndPrintsInOrder(t *testing.T) {
	t.Setenv("GOFLAGS", "")
	manifest := &Manifest{
		Profiles: []Profile{{Name: "host", LLGOArgs: []string{"-flag"}}},
		Cases: []Case{
			{ID: "first", Dir: "_standalone/first", Profiles: []string{"host"}, GOOS: []string{"linux"}},
			{ID: "second", Dir: "_standalone/second", Profiles: []string{"host"}, GOOS: []string{"linux"}},
			{ID: "third", Dir: "_standalone/third", Profiles: []string{"host"}, GOOS: []string{"linux"}},
			{ID: "fourth", Dir: "_standalone/fourth", Profiles: []string{"host"}, GOOS: []string{"linux"}},
		},
	}
	firstStarted := make(chan struct{})
	thirdStarted := make(chan struct{})
	releaseFirst := make(chan struct{})
	var active, maxActive atomic.Int32
	var mu sync.Mutex
	var invocations []string
	command := func(_ context.Context, dir, executable string, args, env []string) ([]byte, []byte, error) {
		current := active.Add(1)
		defer active.Add(-1)
		for {
			old := maxActive.Load()
			if current <= old || maxActive.CompareAndSwap(old, current) {
				break
			}
		}
		name := filepath.Base(dir)
		mu.Lock()
		invocations = append(invocations, strings.Join([]string{
			name, executable, strings.Join(args, " "), envValue(env, "GOTOOLCHAIN"), envValue(env, "GOWORK"), envValue(env, "GOFLAGS"),
		}, "|"))
		mu.Unlock()
		switch name {
		case "first":
			close(firstStarted)
			<-releaseFirst
		case "second":
			<-firstStarted
		case "third":
			close(thirdStarted)
		}
		return []byte(name + " output\n"), nil, nil
	}

	var output bytes.Buffer
	type outcome struct {
		report Report
		err    error
	}
	done := make(chan outcome, 1)
	go func() {
		report, err := Run(context.Background(), manifest, RunOptions{
			Root: "/repo", Profile: "host", GOOS: "linux", Jobs: 2, LLGO: "test-llgo", Out: &output, Command: command,
		})
		done <- outcome{report, err}
	}()
	select {
	case <-thirdStarted:
		// A completed worker received the third case while the first remained blocked.
	case <-time.After(time.Second):
		close(releaseFirst)
		t.Fatal("slow first case blocked scheduling of the third case")
	}
	close(releaseFirst)
	result := <-done
	if result.err != nil || result.report.Failed() != 0 {
		t.Fatalf("Run report = %#v, error = %v", result.report, result.err)
	}
	if got := maxActive.Load(); got != 2 {
		t.Fatalf("maximum concurrency = %d, want 2", got)
	}
	for _, invocation := range invocations {
		parts := strings.Split(invocation, "|")
		if strings.Join(parts[1:], "|") != "test-llgo|run -flag .|local|off|-mod=readonly" {
			t.Fatalf("invocation = %q", invocation)
		}
	}
	assertOrdered(t, output.String(),
		"first output\nPASS _standalone/first\n",
		"second output\nPASS _standalone/second\n",
		"third output\nPASS _standalone/third\n",
		"fourth output\nPASS _standalone/fourth\n",
	)
}

func envValue(env []string, key string) string {
	prefix := key + "="
	for _, item := range env {
		if strings.HasPrefix(item, prefix) {
			return strings.TrimPrefix(item, prefix)
		}
	}
	return ""
}

func TestRunReportsFailuresAndRejectsUnsafeOptions(t *testing.T) {
	manifest := &Manifest{
		Profiles: []Profile{{Name: "host"}},
		Cases:    []Case{{ID: "case", Dir: "_standalone/case", Profiles: []string{"host"}, GOOS: []string{"linux"}}},
	}
	if _, err := Run(context.Background(), manifest, RunOptions{Profile: "host", GOOS: "linux"}); err == nil {
		t.Fatal("Run accepted zero jobs")
	}
	t.Setenv("GOFLAGS", "-mod=vendor")
	report, err := Run(context.Background(), manifest, RunOptions{
		Root: "/repo", Profile: "host", GOOS: "linux", Jobs: 1,
		Command: func(context.Context, string, string, []string, []string) ([]byte, []byte, error) {
			return nil, nil, errors.New("must not run")
		},
	})
	if err != nil || report.Failed() != 1 || !strings.Contains(report.Results[0].Err.Error(), "requires -mod=readonly") {
		t.Fatalf("unsafe GOFLAGS report = %#v, error = %v", report, err)
	}
}

func TestRunBatchBuildContinuesWithProducedExecutables(t *testing.T) {
	t.Setenv("GOFLAGS", "")
	manifest := &Manifest{
		Profiles: []Profile{{Name: "host-lto", LLGOArgs: []string{"-lto=full"}}},
		Cases: []Case{
			{ID: "c-good", Dir: "_demo/c/good", Profiles: []string{"host-lto"}, GOOS: []string{"linux"}},
			{ID: "c-bad", Dir: "_demo/c/bad", Profiles: []string{"host-lto"}, GOOS: []string{"linux"}},
			{ID: "go-good", Dir: "_demo/go/good", Profiles: []string{"host-lto"}, GOOS: []string{"linux"}},
		},
	}
	var mu sync.Mutex
	var buildArgs [][]string
	var ran []string
	command := func(_ context.Context, dir, executable string, args, env []string) ([]byte, []byte, error) {
		if len(args) != 0 && args[0] == "build" {
			mu.Lock()
			buildArgs = append(buildArgs, append([]string(nil), args...))
			mu.Unlock()
			var out string
			for i := range args {
				if args[i] == "-o" {
					out = args[i+1]
				}
			}
			name := "good"
			if filepath.Base(dir) == "go" {
				name = "good"
			}
			if err := os.MkdirAll(out, 0o755); err != nil {
				return nil, nil, err
			}
			if err := os.WriteFile(filepath.Join(out, name), []byte("binary"), 0o755); err != nil {
				return nil, nil, err
			}
			if filepath.Base(dir) == "c" {
				return nil, []byte("c build failed\n"), errors.New("bad link")
			}
			return nil, nil, nil
		}
		mu.Lock()
		ran = append(ran, filepath.Base(dir)+":"+filepath.Base(executable))
		mu.Unlock()
		return []byte(filepath.Base(dir) + " ran\n"), nil, nil
	}

	var output bytes.Buffer
	report, err := Run(context.Background(), manifest, RunOptions{
		Root: "/repo", Profile: "host-lto", GOOS: "linux", Jobs: 2,
		LLGO: "test-llgo", Out: &output, Command: command,
	})
	if err != nil {
		t.Fatal(err)
	}
	if report.Failed() != 1 || len(report.BuildErrors) != 1 || report.Succeeded() {
		t.Fatalf("batch report = %#v", report)
	}
	if len(buildArgs) != 2 {
		t.Fatalf("build invocations = %q, want two groups", buildArgs)
	}
	for _, args := range buildArgs {
		joined := strings.Join(args, " ")
		if !strings.HasPrefix(joined, "build -lto=full -p=1 -o ") {
			t.Fatalf("build args = %q", args)
		}
	}
	sort.Strings(ran)
	if want := []string{"good:good", "good:good"}; !reflect.DeepEqual(ran, want) {
		t.Fatalf("run invocations = %q, want %q", ran, want)
	}
	assertOrdered(t, output.String(), "c build failed\n", "PASS _demo/c/good\n", "FAIL _demo/c/bad:", "PASS _demo/go/good\n")
}

func TestRunBuildsGroupsConcurrentlyWithinGlobalBudgetAndPrintsInOrder(t *testing.T) {
	t.Setenv("GOFLAGS", "")
	manifest := &Manifest{
		Profiles: []Profile{{Name: "host"}},
		Cases: []Case{
			{ID: "c-one", Dir: "_demo/c/cone", Profiles: []string{"host"}, GOOS: []string{"linux"}},
			{ID: "c-two", Dir: "_demo/c/ctwo", Profiles: []string{"host"}, GOOS: []string{"linux"}},
			{ID: "go-one", Dir: "_demo/go/goone", Profiles: []string{"host"}, GOOS: []string{"linux"}},
			{ID: "go-two", Dir: "_demo/go/gotwo", Profiles: []string{"host"}, GOOS: []string{"linux"}},
			{ID: "go-three", Dir: "_demo/go/gothree", Profiles: []string{"host"}, GOOS: []string{"linux"}},
			{ID: "go-four", Dir: "_demo/go/gofour", Profiles: []string{"host"}, GOOS: []string{"linux"}},
			{ID: "py-one", Dir: "_demo/py/pyone", Profiles: []string{"host"}, GOOS: []string{"linux"}},
		},
	}

	started := make(chan struct{}, 3)
	release := make(chan struct{})
	var activeBudget, maxBudget, activeGroups, maxGroups atomic.Int32
	allocations := make(map[string]int)
	tempDirs := make(map[string]string)
	var mu sync.Mutex
	command := func(_ context.Context, dir, _ string, args, env []string) ([]byte, []byte, error) {
		if len(args) == 0 || args[0] != "build" {
			return nil, nil, nil
		}
		group := filepath.Base(dir)
		jobs := 0
		out := ""
		for index, arg := range args {
			if strings.HasPrefix(arg, "-p=") {
				jobs, _ = strconv.Atoi(strings.TrimPrefix(arg, "-p="))
			}
			if arg == "-o" {
				out = args[index+1]
			}
		}
		mu.Lock()
		allocations[group] = jobs
		tempDirs[group] = envValue(env, "TMPDIR")
		mu.Unlock()
		currentBudget := activeBudget.Add(int32(jobs))
		updateMaximum(&maxBudget, currentBudget)
		currentGroups := activeGroups.Add(1)
		updateMaximum(&maxGroups, currentGroups)
		started <- struct{}{}
		<-release
		defer activeBudget.Add(int32(-jobs))
		defer activeGroups.Add(-1)
		for _, arg := range args {
			if !strings.HasPrefix(arg, "./") {
				continue
			}
			if err := os.WriteFile(filepath.Join(out, filepath.Base(arg)), []byte("binary"), 0o755); err != nil {
				return nil, nil, err
			}
		}
		return []byte(group + " build\n"), nil, nil
	}

	type outcome struct {
		report Report
		err    error
	}
	var output bytes.Buffer
	done := make(chan outcome, 1)
	go func() {
		report, err := Run(context.Background(), manifest, RunOptions{
			Root: "/repo", Profile: "host", GOOS: "linux", Jobs: 4,
			LLGO: "test-llgo", Out: &output, Command: command,
		})
		done <- outcome{report, err}
	}()
	for range 3 {
		select {
		case <-started:
		case <-time.After(time.Second):
			close(release)
			t.Fatal("C, Go, and Python group builds did not overlap")
		}
	}
	close(release)
	result := <-done
	if result.err != nil || !result.report.Succeeded() {
		t.Fatalf("Run report = %#v, error = %v", result.report, result.err)
	}
	if got := maxBudget.Load(); got != 4 {
		t.Fatalf("maximum package-build budget = %d, want 4", got)
	}
	if got := maxGroups.Load(); got != 3 {
		t.Fatalf("maximum concurrent groups = %d, want 3", got)
	}
	if want := map[string]int{"c": 1, "go": 2, "py": 1}; !reflect.DeepEqual(allocations, want) {
		t.Fatalf("group allocations = %v, want %v", allocations, want)
	}
	if tempDirs["c"] == tempDirs["go"] || tempDirs["c"] == tempDirs["py"] || tempDirs["go"] == tempDirs["py"] {
		t.Fatalf("group TMPDIR values are not isolated: %v", tempDirs)
	}
	assertOrdered(t, output.String(),
		"Building _demo/c (-p=1)\n",
		"Building _demo/go (-p=2)\n",
		"Building _demo/py (-p=1)\n",
		"c build\n", "go build\n", "py build\n",
		"PASS _demo/c/cone\n", "PASS _demo/go/goone\n", "PASS _demo/py/pyone\n",
	)
}

func updateMaximum(maximum *atomic.Int32, current int32) {
	for {
		old := maximum.Load()
		if current <= old || maximum.CompareAndSwap(old, current) {
			return
		}
	}
}

func BenchmarkRunWorkerPool(b *testing.B) {
	b.Setenv("GOFLAGS", "")
	manifest := &Manifest{Profiles: []Profile{{Name: "host"}}}
	for i := 0; i < 8; i++ {
		manifest.Cases = append(manifest.Cases, Case{
			ID: "case-" + string(rune('a'+i)), Dir: "_standalone/case", Profiles: []string{"host"}, GOOS: []string{"linux"},
		})
	}
	command := func(context.Context, string, string, []string, []string) ([]byte, []byte, error) {
		time.Sleep(10 * time.Millisecond)
		return nil, nil, nil
	}
	for _, test := range []struct {
		name string
		jobs int
	}{{"jobs-1", 1}, {"jobs-4", 4}} {
		b.Run(test.name, func(b *testing.B) {
			for range b.N {
				if _, err := Run(context.Background(), manifest, RunOptions{
					Root: "/repo", Profile: "host", GOOS: "linux", Jobs: test.jobs, Command: command,
				}); err != nil {
					b.Fatal(err)
				}
			}
		})
	}
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
