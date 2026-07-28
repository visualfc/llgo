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
	"errors"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

const benchmarkDataPrefix = "window.BENCHMARK_DATA = "

type actionBenchmarkData struct {
	Entries map[string][]actionBenchmarkSuite `json:"entries"`
}

type actionBenchmarkSuite struct {
	Commit struct {
		ID string `json:"id"`
	} `json:"commit"`
	Benches []actionBenchmark `json:"benches"`
}

type actionBenchmark struct {
	Name  string  `json:"name"`
	Unit  string  `json:"unit"`
	Value float64 `json:"value"`
}

type reportOptions struct {
	currentData string
	mainData    string
	output      string
	seriesURL   string
	sourceURL   string
	runURL      string
	sourceSHA   string
}

func writeBenchmarkReport(options reportOptions) error {
	if options.currentData == "" || options.output == "" || options.seriesURL == "" ||
		options.sourceURL == "" || options.runURL == "" || len(options.sourceSHA) < 12 {
		return errors.New("report mode requires current data, output, series/source/run URLs, and source SHA")
	}
	current, err := readActionBenchmarkData(options.currentData, false)
	if err != nil {
		return err
	}
	main, err := readActionBenchmarkData(options.mainData, true)
	if err != nil {
		return err
	}

	var body strings.Builder
	body.WriteString("<!-- llgo-baseline-benchmark -->\n")
	body.WriteString("## LLGo baseline benchmarks\n\n")
	fmt.Fprintf(&body, "[`%s`](%s) | [workflow run](%s) | [long-term charts](%s)\n\n",
		options.sourceSHA[:12], options.sourceURL, options.runURL, options.seriesURL)
	body.WriteString("### Program measurements\n\n")
	body.WriteString("| Platform | Workload | File size | vs main | Build | vs main | Run | vs main |\n")
	body.WriteString("|---|---|---:|---:|---:|---:|---:|---:|\n")

	for _, platform := range []string{"Linux", "macOS"} {
		sizes, err := suiteForCommit(current, platform+" program binary size", options.sourceSHA)
		if err != nil {
			return err
		}
		timings, err := suiteForCommit(current, platform+" program build and run time", options.sourceSHA)
		if err != nil {
			return err
		}
		mainSizes := latestSuite(main, platform+" program binary size")
		mainTimings := latestSuite(main, platform+" program build and run time")
		for _, item := range workloads {
			fileName := "binary/" + item.name + "/file"
			buildName := "compile/" + item.name
			runName := "run/" + item.name
			fileSize, err := benchmarkValue(sizes, fileName, "bytes")
			if err != nil {
				return err
			}
			buildTime, err := benchmarkValue(timings, buildName, "ns")
			if err != nil {
				return err
			}
			runTime, err := benchmarkValue(timings, runName, "ns")
			if err != nil {
				return err
			}
			fmt.Fprintf(&body, "| %s | `%s` | %s | %s | %s | %s | %s | %s |\n",
				platform,
				item.name,
				formatBytes(fileSize),
				formatDelta(fileSize, optionalBenchmarkValue(mainSizes, fileName, "bytes")),
				formatDuration(buildTime),
				formatDelta(buildTime, optionalBenchmarkValue(mainTimings, buildName, "ns")),
				formatDuration(runTime),
				formatDelta(runTime, optionalBenchmarkValue(mainTimings, runName, "ns")),
			)
		}
	}

	body.WriteString("\n<details>\n<summary>Core language and compiler benchmarks</summary>\n\n")
	body.WriteString("| Platform | Category | Benchmark | Current | vs main |\n")
	body.WriteString("|---|---|---|---:|---:|\n")
	for _, platform := range []string{"Linux", "macOS"} {
		currentSuite, err := suiteForCommit(current, platform+" compiler and core language", options.sourceSHA)
		if err != nil {
			return err
		}
		mainSuite := latestSuite(main, platform+" compiler and core language")
		for _, category := range benchmarkCategoryOrder {
			for _, name := range expectedGoBenchmarks {
				if benchmarkCategory(name) != category {
					continue
				}
				value, err := goBenchmarkValue(currentSuite, name)
				if err != nil {
					return err
				}
				fmt.Fprintf(&body, "| %s | %s | `%s` | %s ns/op | %s |\n",
					platform, category, name, formatNumber(value),
					formatDelta(value, optionalGoBenchmarkValue(mainSuite, name)))
			}
		}
	}
	body.WriteString("\n</details>\n\n")
	body.WriteString("_Generated from allowlist-validated artifacts; this comment is updated in place._\n")
	return os.WriteFile(options.output, []byte(body.String()), 0o644)
}

func readActionBenchmarkData(path string, optional bool) (actionBenchmarkData, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		if optional && os.IsNotExist(err) {
			return actionBenchmarkData{Entries: map[string][]actionBenchmarkSuite{}}, nil
		}
		return actionBenchmarkData{}, err
	}
	if !strings.HasPrefix(string(data), benchmarkDataPrefix) {
		return actionBenchmarkData{}, fmt.Errorf("%s: invalid benchmark data prefix", path)
	}
	var decoded actionBenchmarkData
	if err := json.Unmarshal(data[len(benchmarkDataPrefix):], &decoded); err != nil {
		return actionBenchmarkData{}, fmt.Errorf("%s: %w", path, err)
	}
	if decoded.Entries == nil {
		return actionBenchmarkData{}, fmt.Errorf("%s: missing benchmark entries", path)
	}
	return decoded, nil
}

func suiteForCommit(data actionBenchmarkData, name, sha string) (*actionBenchmarkSuite, error) {
	suites := data.Entries[name]
	for i := len(suites) - 1; i >= 0; i-- {
		if suites[i].Commit.ID == sha {
			return &suites[i], nil
		}
	}
	return nil, fmt.Errorf("benchmark suite %q has no result for %s", name, sha)
}

func latestSuite(data actionBenchmarkData, name string) *actionBenchmarkSuite {
	suites := data.Entries[name]
	if len(suites) == 0 {
		return nil
	}
	return &suites[len(suites)-1]
}

func benchmarkValue(suite *actionBenchmarkSuite, name, unit string) (float64, error) {
	if suite != nil {
		for _, bench := range suite.Benches {
			if bench.Name == name && bench.Unit == unit && validReportValue(bench.Value) {
				return bench.Value, nil
			}
		}
	}
	return 0, fmt.Errorf("benchmark %q (%s) is missing", name, unit)
}

func optionalBenchmarkValue(suite *actionBenchmarkSuite, name, unit string) *float64 {
	value, err := benchmarkValue(suite, name, unit)
	if err != nil {
		return nil
	}
	return &value
}

func goBenchmarkValue(suite *actionBenchmarkSuite, name string) (float64, error) {
	if suite != nil {
		for _, bench := range suite.Benches {
			if goBenchmarkBase(bench.Name) == name && bench.Unit == "ns/op" && validReportValue(bench.Value) {
				return bench.Value, nil
			}
		}
	}
	return 0, fmt.Errorf("Go benchmark %q is missing", name)
}

func optionalGoBenchmarkValue(suite *actionBenchmarkSuite, name string) *float64 {
	value, err := goBenchmarkValue(suite, name)
	if err != nil {
		return nil
	}
	return &value
}

func goBenchmarkBase(name string) string {
	if index := strings.Index(name, " ("); index >= 0 {
		return name[:index]
	}
	return name
}

var benchmarkCategoryOrder = []string{
	"Compiler",
	"Local storage",
	"Calls",
	"Runtime",
	"Goroutines",
	"Channels",
	"Other",
}

func benchmarkCategory(name string) string {
	switch {
	case name == "BenchmarkLookupPCRandom" ||
		name == "BenchmarkMergeCompilerFlags" ||
		name == "BenchmarkMergeLinkerFlags":
		return "Compiler"
	case strings.HasPrefix(name, "BenchmarkGlobal") ||
		strings.HasPrefix(name, "BenchmarkTLS") ||
		strings.HasPrefix(name, "BenchmarkGLS"):
		return "Local storage"
	case name == "BenchmarkDirectCall" || name == "BenchmarkInterfaceCall":
		return "Calls"
	case name == "BenchmarkDefer" || name == "BenchmarkRuntimeGetG":
		return "Runtime"
	case name == "BenchmarkGoroutine":
		return "Goroutines"
	case strings.HasPrefix(name, "BenchmarkChannel"):
		return "Channels"
	default:
		return "Other"
	}
}

func validReportValue(value float64) bool {
	return value >= 0 && !math.IsNaN(value) && !math.IsInf(value, 0)
}

func formatBytes(value float64) string {
	return strconv.FormatInt(int64(math.Round(value)), 10) + " B"
}

func formatDuration(value float64) string {
	switch {
	case value >= 1e9:
		return formatNumber(value/1e9) + " s"
	case value >= 1e6:
		return formatNumber(value/1e6) + " ms"
	case value >= 1e3:
		return formatNumber(value/1e3) + " us"
	default:
		return formatNumber(value) + " ns"
	}
}

func formatDelta(current float64, baseline *float64) string {
	if baseline == nil {
		return "new"
	}
	if *baseline == 0 {
		if current == 0 {
			return "0.0%"
		}
		return "from 0"
	}
	return fmt.Sprintf("%+.1f%%", (current/(*baseline)-1)*100)
}

func formatNumber(value float64) string {
	return strconv.FormatFloat(value, 'f', 3, 64)
}
