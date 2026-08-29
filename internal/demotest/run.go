package demotest

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"time"
)

type CommandFunc func(ctx context.Context, dir, executable string, args, env []string) (stdout, stderr []byte, err error)

type RunOptions struct {
	Root    string
	Profile string
	GOOS    string
	Jobs    int
	LLGO    string
	Out     io.Writer
	Command CommandFunc
	Cases   []string
}

type CaseResult struct {
	Case     PlannedCase
	Stdout   []byte
	Stderr   []byte
	Duration time.Duration
	Err      error
}

type Report struct {
	Profile string
	Results []CaseResult
}

func (report Report) Failed() int {
	failed := 0
	for _, result := range report.Results {
		if result.Err != nil {
			failed++
		}
	}
	return failed
}

func Run(ctx context.Context, manifest *Manifest, options RunOptions) (Report, error) {
	if options.Root == "" {
		options.Root = "."
	}
	if options.GOOS == "" {
		options.GOOS = runtime.GOOS
	}
	if options.Jobs <= 0 {
		return Report{}, fmt.Errorf("jobs must be positive")
	}
	if options.LLGO == "" {
		options.LLGO = "llgo"
	}
	if options.Out == nil {
		options.Out = io.Discard
	}
	if options.Command == nil {
		options.Command = runCommand
	}
	plan, err := Plan(manifest, options.Profile, options.GOOS)
	if err != nil {
		return Report{}, err
	}
	plan, err = FilterPlan(plan, options.Cases)
	if err != nil {
		return Report{}, err
	}

	results := make([]CaseResult, len(plan))
	done := make([]bool, len(plan))
	work := make(chan int)
	completed := make(chan int, max(1, min(options.Jobs, len(plan))))
	var workers sync.WaitGroup
	workerCount := min(options.Jobs, max(1, len(plan)))
	for range workerCount {
		workers.Add(1)
		go func() {
			defer workers.Done()
			for index := range work {
				results[index] = runOne(ctx, plan[index], options)
				completed <- index
			}
		}()
	}
	nextToSchedule := 0
	schedule := func(index int) {
		printCaseStart(options.Out, plan[index])
		work <- index
	}
	for nextToSchedule < workerCount {
		schedule(nextToSchedule)
		nextToSchedule++
	}
	nextToPrint := 0
	for completedCount := 0; completedCount < len(plan); completedCount++ {
		index := <-completed
		done[index] = true
		for nextToPrint < len(plan) && done[nextToPrint] {
			printCaseResult(options.Out, results[nextToPrint])
			results[nextToPrint].Stdout = nil
			results[nextToPrint].Stderr = nil
			nextToPrint++
		}
		if nextToSchedule < len(plan) {
			schedule(nextToSchedule)
			nextToSchedule++
		}
	}
	close(work)
	workers.Wait()
	return Report{Profile: options.Profile, Results: results}, nil
}

func printCaseStart(out io.Writer, planned PlannedCase) {
	fmt.Fprintf(out, "Testing %s", planned.Case.Dir)
	if target := planned.Profile.Target; target != "" {
		fmt.Fprintf(out, " (target=%s)", target)
	}
	fmt.Fprintln(out)
}

func printCaseResult(out io.Writer, result CaseResult) {
	if len(result.Stdout) != 0 {
		out.Write(result.Stdout)
		if result.Stdout[len(result.Stdout)-1] != '\n' {
			fmt.Fprintln(out)
		}
	}
	if len(result.Stderr) != 0 {
		out.Write(result.Stderr)
		if result.Stderr[len(result.Stderr)-1] != '\n' {
			fmt.Fprintln(out)
		}
	}
	if result.Err != nil {
		fmt.Fprintf(out, "FAIL %s: %v\n", result.Case.Case.Dir, result.Err)
	} else {
		fmt.Fprintf(out, "PASS %s\n", result.Case.Case.Dir)
	}
}

func runOne(parent context.Context, planned PlannedCase, options RunOptions) CaseResult {
	ctx, cancel := context.WithTimeout(parent, planned.Timeout)
	defer cancel()

	temp, err := os.MkdirTemp("", "llgo-demo-")
	if err != nil {
		return CaseResult{Case: planned, Err: err}
	}
	defer os.RemoveAll(temp)

	env, err := commandEnvironment(temp)
	if err != nil {
		return CaseResult{Case: planned, Err: err}
	}
	start := time.Now()
	stdout, stderr, runErr := options.Command(
		ctx,
		filepath.Join(options.Root, filepath.FromSlash(planned.Case.Dir)),
		options.LLGO,
		planned.LLGOArguments(),
		env,
	)
	duration := time.Since(start)
	if ctxErr := ctx.Err(); ctxErr != nil {
		if ctxErr == context.DeadlineExceeded {
			runErr = fmt.Errorf("timed out after %s", planned.Timeout)
		} else {
			runErr = fmt.Errorf("demo run canceled: %w", ctxErr)
		}
		return CaseResult{Case: planned, Stdout: stdout, Stderr: stderr, Duration: duration, Err: runErr}
	}
	checkErr := CheckResult(options.Root, planned.Case.Check, stdout, stderr, runErr)
	return CaseResult{Case: planned, Stdout: stdout, Stderr: stderr, Duration: duration, Err: checkErr}
}

func runCommand(ctx context.Context, dir, executable string, args, env []string) ([]byte, []byte, error) {
	cmd := exec.CommandContext(ctx, executable, args...)
	cmd.Dir = dir
	cmd.Env = env
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err := cmd.Run()
	return stdout.Bytes(), stderr.Bytes(), err
}

func commandEnvironment(temp string) ([]string, error) {
	env := os.Environ()
	goFlags := environmentValue(env, "GOFLAGS")
	hasReadonly := false
	for _, flag := range strings.Fields(goFlags) {
		if !strings.HasPrefix(flag, "-mod=") {
			continue
		}
		if flag != "-mod=readonly" {
			return nil, fmt.Errorf("GOFLAGS contains %s; demo runner requires -mod=readonly", flag)
		}
		hasReadonly = true
	}
	if goFlags != "" && !hasReadonly {
		goFlags += " "
	}
	if !hasReadonly {
		goFlags += "-mod=readonly"
	}
	env = setEnvironment(env, "GOTOOLCHAIN", "local")
	env = setEnvironment(env, "GOWORK", "off")
	env = setEnvironment(env, "GOFLAGS", goFlags)
	env = setEnvironment(env, "TMPDIR", temp)
	if runtime.GOOS == "windows" {
		env = setEnvironment(env, "TEMP", temp)
		env = setEnvironment(env, "TMP", temp)
	}
	return env, nil
}

func environmentValue(env []string, key string) string {
	prefix := key + "="
	for _, item := range env {
		if len(item) >= len(prefix) && item[:len(prefix)] == prefix {
			return item[len(prefix):]
		}
	}
	return ""
}

func setEnvironment(env []string, key, value string) []string {
	prefix := key + "="
	filtered := env[:0]
	for _, item := range env {
		if len(item) >= len(prefix) && item[:len(prefix)] == prefix {
			continue
		}
		filtered = append(filtered, item)
	}
	return append(filtered, prefix+value)
}
