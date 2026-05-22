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
	"os/exec"
	"path/filepath"
	"testing"
)

const builtinPrintProbe = `package main

type withMethod interface {
	f()
}

func main() {
	println((interface{})(nil))
	println((withMethod)(nil))
	println((map[int]int)(nil))
	println(([]int)(nil))
	println(int64(-7))
	println(uint64(7), uint32(7), uint16(7), uint8(7), uint(7), uintptr(7))
	println(8.0, complex(9.0, 10.0))
	println(true, false, "hello")
	print("inline: ")
	println("one", "two")

	defer println(13.0, complex(14.0, 15.0))
	defer println(42, true, false, true, 1.5, "world", (chan int)(nil), []int(nil), (map[string]int)(nil), (func())(nil), byte(255))
	defer print("deferred: ")
}
`

func TestBuiltinPrintOutputMatchesGo(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "main.go")
	if err := os.WriteFile(file, []byte(builtinPrintProbe), 0644); err != nil {
		t.Fatal(err)
	}

	repoRoot := findBuiltinPrintRepoRoot(t)
	goBin := filepath.Join(dir, "go-probe")
	llgoBin := filepath.Join(dir, "llgo-probe")
	runBuiltinPrintCommand(t, repoRoot, "go", "build", "-o", goBin, file)
	t.Setenv("LLGO_ROOT", repoRoot)
	runBuiltinPrintCommand(t, repoRoot, "go", "run", "./cmd/llgo", "build", "-o", llgoBin, file)

	want := runBuiltinPrintCommand(t, dir, goBin)
	got := runBuiltinPrintCommand(t, dir, llgoBin)
	if string(got) != string(want) {
		t.Fatalf("llgo print output mismatch\nllgo:\n%s\n\ngo:\n%s", got, want)
	}
}

func runBuiltinPrintCommand(t *testing.T, dir, name string, args ...string) []byte {
	t.Helper()
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	cmd.Env = os.Environ()
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("%s %v failed: %v\n%s", name, args, err, out)
	}
	return out
}

func findBuiltinPrintRepoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatal("repo root not found")
		}
		dir = parent
	}
}
