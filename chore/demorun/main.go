package main

import (
	"context"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"

	"github.com/xgo-dev/llgo/internal/demotest"
)

func main() {
	os.Exit(run(context.Background(), os.Args[1:], os.Stdout, os.Stderr))
}

func run(ctx context.Context, args []string, stdout, stderr io.Writer) int {
	flags := flag.NewFlagSet("demorun", flag.ContinueOnError)
	flags.SetOutput(stderr)
	root := flags.String("root", ".", "repository root")
	manifestPath := flags.String("manifest", "_demo/manifest.json", "manifest path relative to root")
	profile := flags.String("profile", "host", "positive execution profile")
	jobs := flags.Int("jobs", defaultJobs(), "maximum concurrent demo processes")
	resultPath := flags.String("result", "result.md", "markdown result path relative to root; empty disables it")
	llgo := flags.String("llgo", "llgo", "llgo executable")
	checkManifest := flags.Bool("check-manifest", false, "validate ownership without running cases")
	list := flags.Bool("list", false, "list the selected plan without running it")
	var cases stringListFlag
	flags.Var(&cases, "case", "case ID or directory to run; repeat to select multiple cases")
	if err := flags.Parse(args); err != nil {
		return 2
	}
	if flags.NArg() != 0 {
		fmt.Fprintf(stderr, "demorun: unexpected arguments: %v\n", flags.Args())
		return 2
	}

	absoluteRoot, err := filepath.Abs(*root)
	if err != nil {
		fmt.Fprintf(stderr, "demorun: resolve root: %v\n", err)
		return 1
	}
	manifest, err := demotest.LoadManifest(filepath.Join(absoluteRoot, filepath.FromSlash(*manifestPath)))
	if err != nil {
		fmt.Fprintf(stderr, "demorun: %v\n", err)
		return 1
	}
	if err := demotest.Validate(absoluteRoot, manifest); err != nil {
		fmt.Fprintf(stderr, "demorun: invalid manifest:\n%v\n", err)
		return 1
	}
	if *checkManifest {
		fmt.Fprintf(stdout, "manifest OK: %d cases, %d support directories, %d workflow-owned directories\n", len(manifest.Cases), len(manifest.Support), len(manifest.Workflow))
		return 0
	}
	if *list {
		plan, err := demotest.Plan(manifest, *profile, runtime.GOOS)
		if err == nil {
			plan, err = demotest.FilterPlan(plan, cases)
		}
		if err != nil {
			fmt.Fprintf(stderr, "demorun: %v\n", err)
			return 1
		}
		for _, planned := range plan {
			fmt.Fprintf(stdout, "%s\t%s\n", planned.Case.ID, planned.Case.Dir)
		}
		fmt.Fprintf(stdout, "%d cases\n", len(plan))
		return 0
	}

	report, err := demotest.Run(ctx, manifest, demotest.RunOptions{
		Root:    absoluteRoot,
		Profile: *profile,
		GOOS:    runtime.GOOS,
		Jobs:    *jobs,
		LLGO:    *llgo,
		Out:     stdout,
		Cases:   cases,
	})
	if err != nil {
		fmt.Fprintf(stderr, "demorun: %v\n", err)
		return 1
	}
	failed := report.Failed()
	fmt.Fprintln(stdout, "=== Done")
	fmt.Fprintf(stdout, "%d/%d tests passed\n", len(report.Results)-failed, len(report.Results))
	if *resultPath != "" {
		if err := appendResult(filepath.Join(absoluteRoot, filepath.FromSlash(*resultPath)), report); err != nil {
			fmt.Fprintf(stderr, "demorun: write result: %v\n", err)
			return 1
		}
	}
	if failed != 0 || len(report.BuildErrors) != 0 {
		return 1
	}
	return 0
}

func defaultJobs() int {
	return min(max(runtime.NumCPU(), 1), 4)
}

type stringListFlag []string

func (values *stringListFlag) String() string {
	return fmt.Sprint([]string(*values))
}

func (values *stringListFlag) Set(value string) error {
	*values = append(*values, value)
	return nil
}

func appendResult(path string, report demotest.Report) error {
	// Append deliberately: an embedded workflow job runs the ESP32 and
	// ESP32-C3 profiles separately and aggregates both results in one summary.
	f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o666)
	if err != nil {
		return err
	}
	defer f.Close()
	if report.Succeeded() {
		_, err = fmt.Fprintf(f, ":white_check_mark: All demo tests passed (%s)\n", report.Profile)
		return err
	}
	if _, err := fmt.Fprintf(f, ":bangbang: Failed demo cases (%s):\n", report.Profile); err != nil {
		return err
	}
	for _, result := range report.Results {
		if result.Err != nil {
			if _, err := fmt.Fprintf(f, "* :x: %s\n", result.Case.Case.Dir); err != nil {
				return err
			}
		}
	}
	for _, buildErr := range report.BuildErrors {
		if _, err := fmt.Fprintf(f, "* :x: %v\n", buildErr); err != nil {
			return err
		}
	}
	return nil
}
