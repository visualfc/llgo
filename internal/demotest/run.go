package demotest

import (
	"bytes"
	"context"
	"errors"
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

const caseTimeout = 15 * time.Minute

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
	Case   PlannedCase
	Stdout []byte
	Stderr []byte
	Err    error
}

type Report struct {
	Profile     string
	Results     []CaseResult
	BuildErrors []error
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

func (report Report) Succeeded() bool {
	return report.Failed() == 0 && len(report.BuildErrors) == 0
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

	batchRoot, err := os.MkdirTemp("", "llgo-demo-batch-")
	if err != nil {
		return Report{}, err
	}
	defer os.RemoveAll(batchRoot)
	executables, caseBuildErrors, buildErrors := buildBatches(ctx, plan, options, batchRoot)

	results := make([]CaseResult, len(plan))
	done := make([]bool, len(plan))
	work := make(chan int, len(plan))
	completed := make(chan int, len(plan))
	workerCount := min(options.Jobs, max(1, len(plan)))
	for range workerCount {
		go func() {
			for index := range work {
				if caseBuildErrors[index] != nil {
					results[index] = CaseResult{Case: plan[index], Err: caseBuildErrors[index]}
				} else {
					results[index] = runOne(ctx, plan[index], options, executables[index])
				}
				completed <- index
			}
		}()
	}
	for index, planned := range plan {
		printCaseStart(options.Out, planned)
		work <- index
	}
	close(work)
	nextToPrint := 0
	for range plan {
		index := <-completed
		done[index] = true
		for nextToPrint < len(plan) && done[nextToPrint] {
			printCaseResult(options.Out, results[nextToPrint])
			results[nextToPrint].Stdout = nil
			results[nextToPrint].Stderr = nil
			nextToPrint++
		}
	}
	return Report{Profile: options.Profile, Results: results, BuildErrors: buildErrors}, nil
}

func buildBatches(ctx context.Context, plan []PlannedCase, options RunOptions, temp string) ([]string, []error, []error) {
	executables := make([]string, len(plan))
	caseErrors := make([]error, len(plan))
	byGroup := make(map[string]*buildBatch)
	var batches []*buildBatch
	for index, planned := range plan {
		group, ok := batchGroup(planned)
		if !ok {
			continue
		}
		batch := byGroup[group]
		if batch == nil {
			groupTemp := filepath.Join(temp, filepath.Base(group))
			batch = &buildBatch{
				group:   group,
				outDir:  filepath.Join(groupTemp, "bin"),
				tempDir: filepath.Join(groupTemp, "tmp"),
			}
			byGroup[group] = batch
			batches = append(batches, batch)
		}
		batch.indexes = append(batch.indexes, index)
	}
	allocateBatchJobs(batches, options.Jobs)
	for _, batch := range batches {
		fmt.Fprintf(options.Out, "Building %s (-p=%d)\n", batch.group, batch.jobs)
	}
	runBuildBatches(ctx, batches, plan, options)

	var buildErrors []error
	for _, batch := range batches {
		writeOutput(options.Out, batch.stdout)
		writeOutput(options.Out, batch.stderr)
		if batch.err != nil {
			batch.err = fmt.Errorf("build %s: %w", batch.group, batch.err)
			buildErrors = append(buildErrors, batch.err)
		}
		for _, index := range batch.indexes {
			name := filepath.Base(plan[index].Case.Dir)
			if options.GOOS == "windows" {
				name += ".exe"
			}
			executable := filepath.Join(batch.outDir, name)
			if info, err := os.Stat(executable); err == nil && info.Mode().IsRegular() {
				executables[index] = executable
			} else if batch.err != nil {
				caseErrors[index] = fmt.Errorf("output %s was not produced: %w", executable, batch.err)
			} else {
				caseErrors[index] = fmt.Errorf("output %s was not produced", executable)
			}
		}
	}
	return executables, caseErrors, buildErrors
}

type buildBatch struct {
	group           string
	indexes         []int
	jobs            int
	outDir, tempDir string
	stdout, stderr  []byte
	err             error
}

// allocateBatchJobs shares one global package-build budget between groups.
// Every concurrently active group gets at least one slot; remaining slots go
// to the group with the most cases per assigned slot.
func allocateBatchJobs(batches []*buildBatch, jobs int) {
	if len(batches) == 0 {
		return
	}
	for _, batch := range batches {
		batch.jobs = 1
	}
	if jobs < len(batches) {
		return
	}
	for remaining := jobs - len(batches); remaining > 0; remaining-- {
		best := batches[0]
		for _, batch := range batches[1:] {
			if len(batch.indexes)*best.jobs > len(best.indexes)*batch.jobs {
				best = batch
			}
		}
		best.jobs++
	}
}

func runBuildBatches(ctx context.Context, batches []*buildBatch, plan []PlannedCase, options RunOptions) {
	workers := min(len(batches), options.Jobs)
	work := make(chan *buildBatch, len(batches))
	var wg sync.WaitGroup
	for range workers {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for batch := range work {
				runBuildBatch(ctx, batch, plan, options)
			}
		}()
	}
	for _, batch := range batches {
		work <- batch
	}
	close(work)
	wg.Wait()
}

func runBuildBatch(ctx context.Context, batch *buildBatch, plan []PlannedCase, options RunOptions) {
	if err := os.MkdirAll(batch.outDir, 0o755); err != nil {
		batch.err = err
		return
	}
	if err := os.MkdirAll(batch.tempDir, 0o755); err != nil {
		batch.err = err
		return
	}
	patterns := make([]string, 0, len(batch.indexes))
	for _, index := range batch.indexes {
		patterns = append(patterns, "./"+strings.TrimPrefix(plan[index].Case.Dir, batch.group+"/"))
	}
	args := append([]string{"build"}, plan[batch.indexes[0]].Profile.LLGOArgs...)
	args = append(args, fmt.Sprintf("-p=%d", batch.jobs), "-o", batch.outDir+string(os.PathSeparator))
	args = append(args, patterns...)
	env, err := commandEnvironment(batch.tempDir)
	if err != nil {
		batch.err = err
		return
	}
	buildCtx, cancel := context.WithTimeout(ctx, caseTimeout)
	defer cancel()
	batch.stdout, batch.stderr, batch.err = options.Command(buildCtx,
		filepath.Join(options.Root, filepath.FromSlash(batch.group)), options.LLGO, args, env)
	if errors.Is(buildCtx.Err(), context.DeadlineExceeded) {
		batch.err = fmt.Errorf("timed out after %s", caseTimeout)
	}
}

func batchGroup(planned PlannedCase) (string, bool) {
	if planned.Profile.Target != "" || planned.Profile.Name == "model" {
		return "", false
	}
	parts := strings.Split(planned.Case.Dir, "/")
	if len(parts) < 3 || parts[0] != "_demo" || (parts[1] != "c" && parts[1] != "go" && parts[1] != "py") {
		return "", false
	}
	return strings.Join(parts[:2], "/"), true
}

func printCaseStart(out io.Writer, planned PlannedCase) {
	fmt.Fprintf(out, "Testing %s", planned.Case.Dir)
	if target := planned.Profile.Target; target != "" {
		fmt.Fprintf(out, " (target=%s)", target)
	}
	fmt.Fprintln(out)
}

func printCaseResult(out io.Writer, result CaseResult) {
	writeOutput(out, result.Stdout)
	writeOutput(out, result.Stderr)
	if result.Err != nil {
		fmt.Fprintf(out, "FAIL %s: %v\n", result.Case.Case.Dir, result.Err)
	} else {
		fmt.Fprintf(out, "PASS %s\n", result.Case.Case.Dir)
	}
}

func writeOutput(out io.Writer, data []byte) {
	if len(data) == 0 {
		return
	}
	out.Write(data)
	if data[len(data)-1] != '\n' {
		fmt.Fprintln(out)
	}
}

func runOne(parent context.Context, planned PlannedCase, options RunOptions, executable string) CaseResult {
	ctx, cancel := context.WithTimeout(parent, caseTimeout)
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
	args := []string(nil)
	if executable == "" {
		executable = options.LLGO
		args = planned.LLGOArguments()
	}
	stdout, stderr, runErr := options.Command(ctx,
		filepath.Join(options.Root, filepath.FromSlash(planned.Case.Dir)), executable, args, env)
	if ctxErr := ctx.Err(); ctxErr != nil {
		if errors.Is(ctxErr, context.DeadlineExceeded) {
			runErr = fmt.Errorf("timed out after %s", caseTimeout)
		} else {
			runErr = fmt.Errorf("demo run canceled: %w", ctxErr)
		}
		return CaseResult{Case: planned, Stdout: stdout, Stderr: stderr, Err: runErr}
	}
	if runErr != nil {
		runErr = fmt.Errorf("expected successful exit: %w", runErr)
	}
	return CaseResult{Case: planned, Stdout: stdout, Stderr: stderr, Err: runErr}
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
	goFlags := os.Getenv("GOFLAGS")
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
