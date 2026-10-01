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

package gdb

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestGDBIntegration(t *testing.T) {
	if os.Getenv("LLGO_GDB_INTEGRATION") == "" {
		t.Skip("set LLGO_GDB_INTEGRATION=1 to run the native GDB acceptance test")
	}
	if runtime.GOOS != "linux" {
		t.Skip("native GDB acceptance currently runs on Linux")
	}

	gdbPath := integrationTool(t, os.Getenv("LLGO_GDB"), "gdb-multiarch", "gdb")
	llgoPath := integrationTool(t, os.Getenv("LLGO"), "llgo")
	root := integrationRepoRoot(t)
	fixtureDir := filepath.Join(root, "test", "debug", "runtime")
	source := filepath.Join(fixtureDir, "main.go")
	executable := filepath.Join(t.TempDir(), "debug.out")

	build := exec.Command(
		llgoPath, "build", "-O0", "-ldflags=-w=false",
		"-o", executable, ".",
	)
	build.Dir = fixtureDir
	build.Env = append(os.Environ(), "LLGO_ROOT="+root)
	if output, err := build.CombinedOutput(); err != nil {
		t.Fatalf("build GDB fixture: %v\n%s", err, output)
	}

	runtimeLine := integrationMarkerLine(t, source, "LLDB_BREAK: runtime_values")
	interfaceLine := integrationMarkerLine(t, source, "LLDB_BREAK: interface_values")
	functionLine := integrationMarkerLine(t, source, "LLDB_BREAK: function_values")
	containerLine := integrationMarkerLine(t, source, "LLDB_BREAK: container_values")
	args := []string{
		"--nx", "--quiet", "--batch", executable,
		"-ex", "set debuginfod enabled off",
		// The fixture collects while pthreads are live; deliver Boehm's
		// Linux handshake signals without stopping before the source break.
		"-ex", "handle SIGPWR SIGXCPU nostop noprint pass",
		"-ex", fmt.Sprintf("break %s:%d", source, runtimeLine),
		"-ex", fmt.Sprintf("break %s:%d", source, interfaceLine),
		"-ex", fmt.Sprintf("break %s:%d", source, functionLine),
		"-ex", fmt.Sprintf("break %s:%d", source, containerLine),
		"-ex", "break main.InspectGoroutineValues",
		"-ex", "run",
		"-ex", "echo LLGO_CASE=runtime\\n",
		"-ex", "llgo status",
		"-ex", "p text",
		"-ex", "p binary",
		"-ex", "p ints",
		"-ex", "p namedInts",
		"-ex", "continue",
		"-ex", "echo LLGO_CASE=interface\\n",
		"-ex", "p nilAny",
		"-ex", "p anyInt",
		"-ex", "p anyText",
		"-ex", "p nilFoo",
		"-ex", "p foo",
		"-ex", "p err",
		"-ex", "continue",
		"-ex", "echo LLGO_CASE=function\\n",
		"-ex", "p plain",
		"-ex", "p closure",
		"-ex", "p bound",
		"-ex", "p nilFunc",
		"-ex", "continue",
		"-ex", "echo LLGO_CASE=container\\n",
		"-ex", "set print elements 64",
		"-ex", "p nilMap",
		"-ex", "p single",
		"-ex", "p named",
		"-ex", "p many",
		"-ex", "p queued",
		"-ex", "p namedChannel",
		"-ex", "p closedChannel",
		"-ex", "continue",
		"-ex", "echo LLGO_CASE=goroutine\\n",
		"-ex", "llgo goroutines",
		"-ex", "python original_thread = gdb.selected_thread(); [gdb.execute('llgo goroutine %d bt' % item['goid']) for item in _goroutines()]; print('LLGO_THREAD_PRESERVED=' + str(gdb.selected_thread() == original_thread))",
		"-ex", "llgo goroutine 1 bt 3",
	}
	output := integrationRunGDB(t, gdbPath, fixtureDir, args...)
	for _, expected := range []string{
		"LLGO_CASE=runtime",
		"LLGo debugger schema v1 (runtime layout v2)",
		`= "hello"`,
		`= "a\000b"`,
		"len=2 cap=4 = {7, 8}",
		"len=4 cap=4 = {11, 12, 13, 14}",
		"LLGO_CASE=interface",
		"= nil",
		"= type=int",
		"= type=string",
		"= type=*main.Struct",
		"= type=*errors.errorString",
		"LLGO_CASE=function",
		"= main.Plain",
		"main.RuntimeFunctionValues$1 (closure)",
		"main.(*Counter).Add$bound (bound method)",
		"LLGO_CASE=container",
		`len=1 = {["answer"] = 42}`,
		`len=1 = {["named"] = 17}`,
		"len=24 = {",
		"len=2 cap=4 = {8, 9}",
		"len=1 cap=2 = {31}",
		`len=1 cap=2 closed = {"remaining"}`,
		"LLGO_CASE=goroutine",
		"main.InspectGoroutineValues",
		"main.RuntimeGoroutineValues",
		"LLGO_THREAD_PRESERVED=True",
		"goroutine 1 [running] parent=0",
		"tid=",
		"main.InspectGoroutineValues",
	} {
		if !strings.Contains(output, expected) {
			t.Fatalf("GDB output missing %q:\n%s", expected, output)
		}
	}

	integrationTestFallback(t, gdbPath)
}

func integrationTestFallback(t *testing.T, gdbPath string) {
	cc := integrationTool(t, "cc")
	dir := t.TempDir()
	source := filepath.Join(dir, "fallback.c")
	executable := filepath.Join(dir, "fallback")
	code := `
typedef struct { const char *data; unsigned long len; } string;
string cstring = {"raw", 3};
#ifdef LLGO_MARKER_V2
__attribute__((used)) int __llgo_debugger_marker_v2 = 2;
#endif
#ifdef LLGO_BAD_RECORD
__attribute__((used)) int __llgo_debugger_marker_v1 = 1;
__attribute__((used, visibility("hidden")))
unsigned char __llgo_debugger_abi_v1[16] = {
	0x4c, 0x4c, 0x47, 0x4f, 0x44, 0x42, 0x47, 0,
	1, 2, 2, 1, 0, sizeof(void *), 1, 0
};
#endif
int main(void) { return 0; }
`
	if err := os.WriteFile(source, []byte(code), 0600); err != nil {
		t.Fatal(err)
	}
	compile := exec.Command(cc, "-g", "-o", executable, source)
	if output, err := compile.CombinedOutput(); err != nil {
		t.Fatalf("compile fallback fixture: %v\n%s", err, output)
	}
	output := integrationRunGDB(
		t, gdbPath, dir,
		"--nx", "--quiet", "--batch", executable,
		"-ex", "llgo status",
		"-ex", "p cstring",
		"-ex", "p 1+1",
	)
	for _, expected := range []string{
		"Not an LLGo target; raw GDB debugging remains available.",
		`= {data = 0x`,
		`len = 3}`,
		`= 2`,
	} {
		if !strings.Contains(output, expected) {
			t.Fatalf("non-LLGo fallback output missing %q:\n%s", expected, output)
		}
	}

	compile = exec.Command(cc, "-g", "-DLLGO_MARKER_V2", "-o", executable, source)
	if output, err := compile.CombinedOutput(); err != nil {
		t.Fatalf("compile unsupported marker fixture: %v\n%s", err, output)
	}
	output = integrationRunGDB(
		t, gdbPath, dir,
		"--nx", "--quiet", "--batch", executable,
		"-ex", "llgo status",
		"-ex", "p 1+1",
	)
	for _, expected := range []string{
		"Unsupported LLGo debugger marker version(s): v2",
		"raw GDB debugging remains available.",
		`= 2`,
	} {
		if !strings.Contains(output, expected) {
			t.Fatalf("unsupported marker output missing %q:\n%s", expected, output)
		}
	}

	compile = exec.Command(cc, "-g", "-DLLGO_BAD_RECORD", "-o", executable, source)
	if output, err := compile.CombinedOutput(); err != nil {
		t.Fatalf("compile unsupported record fixture: %v\n%s", err, output)
	}
	output = integrationRunGDB(
		t, gdbPath, dir,
		"--nx", "--quiet", "--batch", executable,
		"-ex", "llgo status",
		"-ex", "p 1+1",
	)
	for _, expected := range []string{
		"Unsupported LLGo debugger ABI",
		"unsupported record/schema/runtime/ABI versions",
		"raw GDB debugging remains available.",
		`= 2`,
	} {
		if !strings.Contains(output, expected) {
			t.Fatalf("unsupported record output missing %q:\n%s", expected, output)
		}
	}
}

func integrationRunGDB(t *testing.T, gdbPath, dir string, args ...string) string {
	t.Helper()
	var stdout, stderr bytes.Buffer
	oldDir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}
	defer func() {
		if err := os.Chdir(oldDir); err != nil {
			t.Errorf("restore working directory: %v", err)
		}
	}()
	if err := Run(gdbPath, nil, args, strings.NewReader(""), &stdout, &stderr); err != nil {
		t.Fatalf("run GDB: %v\nstdout:\n%s\nstderr:\n%s", err, stdout.String(), stderr.String())
	}
	return stdout.String() + stderr.String()
}

func integrationMarkerLine(t *testing.T, path, marker string) int {
	t.Helper()
	file, err := os.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	for line := 1; scanner.Scan(); line++ {
		if strings.Contains(scanner.Text(), marker) {
			return line
		}
	}
	if err := scanner.Err(); err != nil {
		t.Fatal(err)
	}
	t.Fatalf("marker %q not found in %s", marker, path)
	return 0
}

func integrationTool(t *testing.T, candidates ...string) string {
	t.Helper()
	for _, candidate := range candidates {
		if candidate == "" {
			continue
		}
		if path, err := exec.LookPath(candidate); err == nil {
			return path
		}
	}
	t.Fatalf("required integration tool not found: %s", strings.Join(candidates, ", "))
	return ""
}

func integrationRepoRoot(t *testing.T) string {
	t.Helper()
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("locate integration test source")
	}
	return filepath.Clean(filepath.Join(filepath.Dir(file), "..", "..", ".."))
}
