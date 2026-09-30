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
	"bytes"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestGDBSourceCommandEscapesPath(t *testing.T) {
	got := gdbSourceCommand(`a\b"c.py`)
	want := `source a\\b\"c.py`
	if got != want {
		t.Fatalf("gdbSourceCommand() = %q, want %q", got, want)
	}
}

func TestFindGDBPrecedenceAndVersion(t *testing.T) {
	newGDB := writeFakeGDB(t, "GNU gdb (GDB) 15.1", "")
	oldGDB := writeFakeGDB(t, "GNU gdb (GDB) 11.2", "")

	got, err := findGDBFrom(newGDB, oldGDB, []string{oldGDB}, nil)
	if err != nil || got != newGDB {
		t.Fatalf("configured GDB = %q, %v; want %q", got, err, newGDB)
	}
	if _, err := findGDBFrom(oldGDB, "", nil, nil); err == nil || !strings.Contains(err.Error(), "version 12 or newer") {
		t.Fatalf("old configured GDB error = %v", err)
	}
	got, err = findGDBFrom("", "", []string{oldGDB, newGDB}, nil)
	if err != nil || got != newGDB {
		t.Fatalf("candidate GDB = %q, %v; want %q", got, err, newGDB)
	}
}

func TestRunEmbedsPluginAndSchema(t *testing.T) {
	fake := writeFakeGDB(t, "GNU gdb (Ubuntu 15.0.50) 15.0.50", `
for arg in "$@"; do
  case "$arg" in
    source\ *) plugin=${arg#source } ;;
  esac
done
test -f "$plugin"
schema=$(dirname "$plugin")/llgo_debugger_schema_v1.json
grep -q '"contract": "llgo.debugger"' "$schema"
grep -q 'class LLGoStatus' "$plugin"
printf 'LLGO_FAKE_GDB_OK\n'
`)
	var stdout, stderr bytes.Buffer
	if err := Run(fake, nil, []string{"--batch", "debug.out"}, strings.NewReader(""), &stdout, &stderr); err != nil {
		t.Fatalf("Run() error: %v; stderr=%s", err, stderr.String())
	}
	if !strings.Contains(stdout.String(), "LLGO_FAKE_GDB_OK") {
		t.Fatalf("Run() output = %q", stdout.String())
	}
}

func writeFakeGDB(t *testing.T, version, body string) string {
	t.Helper()
	if runtime.GOOS == "windows" {
		t.Skip("fake shell debugger is Unix-only")
	}
	path := filepath.Join(t.TempDir(), "gdb")
	script := `#!/bin/sh
if [ "$1" = "--version" ]; then
  printf '%s\n' '` + version + `'
  exit 0
fi
if [ "$1" = "--batch" ] && [ "$2" = "--nx" ]; then
  exit 0
fi
` + body
	if err := os.WriteFile(path, []byte(script), 0700); err != nil {
		t.Fatal(err)
	}
	return path
}
