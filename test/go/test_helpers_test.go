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
	"os"
	"path/filepath"
	"runtime"
	"testing"
)

const llgoTestCompilerEnv = "LLGO_TEST_COMPILER"

func configuredLLGoTestCompiler(t *testing.T) string {
	t.Helper()
	compiler := os.Getenv(llgoTestCompilerEnv)
	if compiler == "" {
		return ""
	}
	abs, err := filepath.Abs(compiler)
	if err != nil {
		t.Fatalf("resolve %s: %v", llgoTestCompilerEnv, err)
	}
	info, err := os.Stat(abs)
	if err != nil {
		t.Fatalf("stat %s: %v", llgoTestCompilerEnv, err)
	}
	if info.IsDir() {
		t.Fatalf("%s points to a directory: %s", llgoTestCompilerEnv, abs)
	}
	return abs
}

func TestConfiguredLLGoTestCompiler(t *testing.T) {
	t.Setenv(llgoTestCompilerEnv, "")
	if got := configuredLLGoTestCompiler(t); got != "" {
		t.Fatalf("empty %s resolved to %q", llgoTestCompilerEnv, got)
	}

	executable, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv(llgoTestCompilerEnv, executable)
	want, err := filepath.Abs(executable)
	if err != nil {
		t.Fatal(err)
	}
	if got := configuredLLGoTestCompiler(t); got != want {
		t.Fatalf("configured compiler = %q, want %q", got, want)
	}
}

func testExecutablePath(dir, name string) string {
	if runtime.GOOS == "windows" {
		name += ".exe"
	}
	return filepath.Join(dir, name)
}
