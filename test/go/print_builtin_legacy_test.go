//go:build !go1.26
// +build !go1.26

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

package gotest

import (
	"bytes"
	"os"
	"os/exec"
	"strings"
	"testing"
)

const (
	printComplexReal = 5
	printComplexImag = 6i

	printComplexValue = printComplexReal + printComplexImag
)

func printComplexLegacyArg(c complex128) {
	println(c)
}

func TestBuiltinPrintComplexLegacyHelper(t *testing.T) {
	if os.Getenv("LLGO_PRINT_COMPLEX_LEGACY_HELPER") == "" {
		t.Skip("helper process")
	}

	println(printComplexValue)
	printComplexLegacyArg(printComplexValue)

	c := printComplexValue
	println(c)
	printComplexLegacyArg(c)
}

func TestBuiltinPrintComplexLegacyExponentWidth(t *testing.T) {
	cmd := exec.Command(os.Args[0], "-test.run=^TestBuiltinPrintComplexLegacyHelper$")
	cmd.Env = append(os.Environ(), "LLGO_PRINT_COMPLEX_LEGACY_HELPER=1")

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("builtin print complex probe failed: %v\nstdout:\n%s\nstderr:\n%s", err, stdout.String(), stderr.String())
	}

	got := strings.ReplaceAll(stderr.String(), "\r\n", "\n")
	want := "" +
		"(+5.000000e+000+6.000000e+000i)\n" +
		"(+5.000000e+000+6.000000e+000i)\n" +
		"(+5.000000e+000+6.000000e+000i)\n" +
		"(+5.000000e+000+6.000000e+000i)\n"
	if got != want {
		t.Fatalf("builtin print complex output mismatch:\n got %q\nwant %q", got, want)
	}
}
