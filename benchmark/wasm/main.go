/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"slices"
	"sort"
	"strings"
	"time"
)

type wasmProfile struct {
	name      string
	target    string
	goos      string
	outputExt string
	hasJSGlue bool
}

var wasmProfiles = []wasmProfile{
	// GOOS/GOARCH entries measure the current implementation without
	// claiming the official-Go ABI contract assigned to the later G1/G2 work.
	{name: "js", goos: "js", outputExt: ".mjs", hasJSGlue: true},
	{name: "wasip1", goos: "wasip1", outputExt: ".wasm"},
	{name: "ec32", target: "emscripten", outputExt: ".mjs", hasJSGlue: true},
	{name: "ec64", target: "emscripten-memory64", outputExt: ".mjs", hasJSGlue: true},
	{name: "wc32", target: "wasi", outputExt: ".wasm"},
}

type commandRunner func(context.Context, string, []string, string, ...string) error

type measurement struct {
	name        string
	moduleBytes int64
	glueBytes   int64
	build       time.Duration
}

func main() {
	os.Exit(runMain(context.Background(), os.Stderr, os.Args[1:], runCommand))
}

func runMain(ctx context.Context, stderr io.Writer, args []string, runner commandRunner) int {
	if err := runCLI(ctx, args, runner); err != nil {
		fmt.Fprintln(stderr, err)
		return 1
	}
	return 0
}

func runCLI(ctx context.Context, args []string, runner commandRunner) error {
	flags := flag.NewFlagSet("llgo-wasm-benchmark", flag.ContinueOnError)
	flags.SetOutput(io.Discard)
	root := flags.String("root", ".", "LLGo repository root")
	llgo := flags.String("llgo", "llgo", "LLGo command")
	out := flags.String("out", filepath.Join("benchmark", "wasm", "out"), "result directory")
	buildRuns := flags.Int("build-runs", 3, "build repetitions per profile")
	if err := flags.Parse(args); err != nil {
		return err
	}
	if *buildRuns <= 0 {
		return errors.New("build repetitions must be positive")
	}

	absRoot, err := filepath.Abs(*root)
	if err != nil {
		return err
	}
	absOut, err := filepath.Abs(*out)
	if err != nil {
		return err
	}
	if err := os.RemoveAll(absOut); err != nil {
		return err
	}
	if err := os.MkdirAll(absOut, 0o755); err != nil {
		return err
	}

	env := append(os.Environ(),
		"GOMAXPROCS=2",
		"LLGO_ROOT="+absRoot,
		"LLGO_BUILD_CACHE=off",
	)
	fixture := filepath.Join(absRoot, "benchmark", "binary_size", "println", "main.go")
	measurements := make([]measurement, 0, len(wasmProfiles))
	for _, profile := range wasmProfiles {
		result, err := measureProfile(ctx, runner, env, absRoot, *llgo, absOut, fixture, profile, *buildRuns)
		if err != nil {
			return fmt.Errorf("build %s: %w", profile.name, err)
		}
		measurements = append(measurements, result)
	}
	return writeResults(filepath.Join(absOut, "benchmark.txt"), measurements)
}

func measureProfile(
	ctx context.Context,
	runner commandRunner,
	env []string,
	root, llgo, out, fixture string,
	profile wasmProfile,
	buildRuns int,
) (measurement, error) {
	profileDir := filepath.Join(out, "bin", profile.name)
	output := filepath.Join(profileDir, "program"+profile.outputExt)
	args := []string{"build"}
	if profile.target != "" {
		args = append(args, "-target", profile.target)
	}
	args = append(args, "-o", output, fixture)
	profileEnv := slices.Clone(env)
	if profile.goos != "" {
		profileEnv = append(profileEnv, "GOOS="+profile.goos, "GOARCH=wasm")
	}

	build := func() error {
		if err := os.RemoveAll(profileDir); err != nil {
			return err
		}
		if err := os.MkdirAll(profileDir, 0o755); err != nil {
			return err
		}
		return runner(ctx, root, profileEnv, llgo, args...)
	}
	// Keep first-use filesystem and host-tool caches outside the samples.
	if err := build(); err != nil {
		return measurement{}, fmt.Errorf("warm build: %w", err)
	}
	durations := make([]time.Duration, 0, buildRuns)
	for range buildRuns {
		start := time.Now()
		if err := build(); err != nil {
			return measurement{}, err
		}
		durations = append(durations, time.Since(start))
	}

	module := output
	glueBytes := int64(0)
	if profile.hasJSGlue {
		glue, err := os.Stat(output)
		if err != nil {
			return measurement{}, fmt.Errorf("inspect JS glue: %w", err)
		}
		if glue.Size() == 0 {
			return measurement{}, errors.New("generated empty JS glue")
		}
		glueBytes = glue.Size()
		module = output[:len(output)-len(profile.outputExt)] + ".wasm"
	}
	moduleBytes, err := wasmModuleSize(module)
	if err != nil {
		return measurement{}, err
	}
	return measurement{
		name:        profile.name,
		moduleBytes: moduleBytes,
		glueBytes:   glueBytes,
		build:       medianDuration(durations),
	}, nil
}

func wasmModuleSize(path string) (int64, error) {
	f, err := os.Open(path)
	if err != nil {
		return 0, fmt.Errorf("inspect wasm module: %w", err)
	}
	defer f.Close()
	var magic [4]byte
	if _, err := io.ReadFull(f, magic[:]); err != nil {
		return 0, fmt.Errorf("read wasm module: %w", err)
	}
	if magic != [4]byte{0, 'a', 's', 'm'} {
		return 0, fmt.Errorf("%s is not a WebAssembly module", path)
	}
	info, err := f.Stat()
	if err != nil {
		return 0, err
	}
	return info.Size(), nil
}

func medianDuration(values []time.Duration) time.Duration {
	values = slices.Clone(values)
	sort.Slice(values, func(i, j int) bool { return values[i] < values[j] })
	return values[len(values)/2]
}

func writeResults(path string, measurements []measurement) error {
	var output strings.Builder
	fmt.Fprintf(&output, "goos: %s\ngoarch: %s\npkg: github.com/xgo-dev/llgo/benchmark/wasm\n", runtime.GOOS, runtime.GOARCH)
	for _, unit := range []string{"module-bytes", "glue-bytes"} {
		fmt.Fprintf(&output, "Unit %s better=lower assume=exact\n", unit)
	}
	fmt.Fprintln(&output, "Unit build-ns better=lower")
	for _, result := range measurements {
		fmt.Fprintf(
			&output,
			"BenchmarkWasmBuild/%s 1 %d module-bytes %d glue-bytes %d build-ns\n",
			result.name,
			result.moduleBytes,
			result.glueBytes,
			result.build.Nanoseconds(),
		)
	}
	return os.WriteFile(path, []byte(output.String()), 0o644)
}

func runCommand(ctx context.Context, dir string, env []string, name string, args ...string) error {
	cmd := exec.CommandContext(ctx, name, args...)
	cmd.Dir = dir
	cmd.Env = env
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
