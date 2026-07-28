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
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

const reportTestSHA = "0123456789abcdef0123456789abcdef01234567"

func TestWriteBenchmarkReport(t *testing.T) {
	dir := t.TempDir()
	current := filepath.Join(dir, "current.js")
	main := filepath.Join(dir, "main.js")
	writeActionData(t, current, reportTestSHA, 110)
	writeActionData(t, main, "main", 100)
	output := filepath.Join(dir, "comment.md")
	err := writeBenchmarkReport(reportOptions{
		currentData: current,
		mainData:    main,
		output:      output,
		seriesURL:   "https://example.com/series",
		sourceURL:   "https://example.com/commit",
		runURL:      "https://example.com/run",
		sourceSHA:   reportTestSHA,
	})
	if err != nil {
		t.Fatal(err)
	}
	data, err := os.ReadFile(output)
	if err != nil {
		t.Fatal(err)
	}
	text := string(data)
	for _, want := range []string{
		"<!-- llgo-baseline-benchmark -->",
		"[`0123456789ab`](https://example.com/commit)",
		"| Linux | `cprintf` | 110 B | +10.0%",
		"| macOS | Runtime | `BenchmarkRuntimeGetG` | 110.000 ns/op | +10.0% |",
		"<summary>Core language and compiler benchmarks</summary>",
	} {
		if !strings.Contains(text, want) {
			t.Fatalf("report does not contain %q:\n%s", want, text)
		}
	}
}

func TestWriteBenchmarkReportWithoutMain(t *testing.T) {
	for _, test := range []struct {
		name       string
		createMain bool
	}{
		{name: "missing"},
		{name: "empty", createMain: true},
	} {
		t.Run(test.name, func(t *testing.T) {
			dir := t.TempDir()
			current := filepath.Join(dir, "current.js")
			main := filepath.Join(dir, "main.js")
			writeActionData(t, current, reportTestSHA, 1)
			if test.createMain {
				writeEmptyActionData(t, main)
			}
			output := filepath.Join(dir, "comment.md")
			err := writeBenchmarkReport(reportOptions{
				currentData: current,
				mainData:    main,
				output:      output,
				seriesURL:   "series",
				sourceURL:   "source",
				runURL:      "run",
				sourceSHA:   reportTestSHA,
			})
			if err != nil {
				t.Fatal(err)
			}
			data, err := os.ReadFile(output)
			if err != nil {
				t.Fatal(err)
			}
			if !strings.Contains(string(data), "| new |") {
				t.Fatalf("report without a baseline has no new delta:\n%s", data)
			}
		})
	}
}

func TestWriteBenchmarkReportRejectsInvalidInput(t *testing.T) {
	if err := writeBenchmarkReport(reportOptions{}); err == nil {
		t.Fatal("writeBenchmarkReport unexpectedly accepted empty options")
	}
	dir := t.TempDir()
	path := filepath.Join(dir, "bad.js")
	if err := os.WriteFile(path, []byte("{}"), 0o644); err != nil {
		t.Fatal(err)
	}
	err := writeBenchmarkReport(reportOptions{
		currentData: path,
		output:      filepath.Join(dir, "out"),
		seriesURL:   "series",
		sourceURL:   "source",
		runURL:      "run",
		sourceSHA:   reportTestSHA,
	})
	if err == nil || !strings.Contains(err.Error(), "prefix") {
		t.Fatalf("writeBenchmarkReport error = %v", err)
	}
}

func TestReportHelpers(t *testing.T) {
	if got := formatDuration(2e9); got != "2.000 s" {
		t.Fatalf("formatDuration seconds = %q", got)
	}
	if got := formatDuration(2e6); got != "2.000 ms" {
		t.Fatalf("formatDuration milliseconds = %q", got)
	}
	if got := formatDuration(2e3); got != "2.000 us" {
		t.Fatalf("formatDuration microseconds = %q", got)
	}
	if got := formatDuration(2); got != "2.000 ns" {
		t.Fatalf("formatDuration nanoseconds = %q", got)
	}
	if got := goBenchmarkBase("BenchmarkOne (example/pkg)"); got != "BenchmarkOne" {
		t.Fatalf("goBenchmarkBase = %q", got)
	}
	if got := formatDelta(1, nil); got != "new" {
		t.Fatalf("formatDelta without baseline = %q", got)
	}
	zero := 0.0
	if got := formatDelta(0, &zero); got != "0.0%" {
		t.Fatalf("formatDelta with two zero values = %q", got)
	}
	if got := formatDelta(1, &zero); got != "from 0" {
		t.Fatalf("formatDelta from zero = %q", got)
	}
	if got := benchmarkCategory("BenchmarkFutureFeature"); got != "Other" {
		t.Fatalf("benchmarkCategory for a future benchmark = %q", got)
	}
}

func writeEmptyActionData(t *testing.T, path string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(benchmarkDataPrefix+`{"entries":{}}`), 0o644); err != nil {
		t.Fatal(err)
	}
}

func writeActionData(t *testing.T, path, sha string, value float64) {
	t.Helper()
	data := actionBenchmarkData{Entries: make(map[string][]actionBenchmarkSuite)}
	for _, platform := range []string{"Linux", "macOS"} {
		size := actionBenchmarkSuite{}
		size.Commit.ID = sha
		timing := actionBenchmarkSuite{}
		timing.Commit.ID = sha
		for _, item := range workloads {
			for _, part := range []string{"file", "text", "data", "bss"} {
				size.Benches = append(size.Benches, actionBenchmark{
					Name: "binary/" + item.name + "/" + part, Unit: "bytes", Value: value,
				})
			}
			timing.Benches = append(timing.Benches,
				actionBenchmark{Name: "compile/" + item.name, Unit: "ns", Value: value},
				actionBenchmark{Name: "run/" + item.name, Unit: "ns", Value: value},
			)
		}
		core := actionBenchmarkSuite{}
		core.Commit.ID = sha
		for _, name := range expectedGoBenchmarks {
			core.Benches = append(core.Benches, actionBenchmark{
				Name: name + " (github.com/goplus/llgo/test)", Unit: "ns/op", Value: value,
			})
		}
		data.Entries[platform+" program binary size"] = []actionBenchmarkSuite{size}
		data.Entries[platform+" program build and run time"] = []actionBenchmarkSuite{timing}
		data.Entries[platform+" compiler and core language"] = []actionBenchmarkSuite{core}
	}
	encoded, err := json.Marshal(data)
	if err != nil {
		t.Fatal(err)
	}
	encoded = append([]byte(benchmarkDataPrefix), encoded...)
	if err := os.WriteFile(path, encoded, 0o644); err != nil {
		t.Fatal(err)
	}
}
