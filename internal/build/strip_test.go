//go:build !llgo

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

package build

import (
	"debug/elf"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
	"testing"

	"github.com/goplus/llgo/xtool/env/llvm"
)

func TestDwarfStripArgs(t *testing.T) {
	tests := []struct {
		name string
		opts linkFlagOptions
		want []string
	}{
		{name: "default"},
		{name: "w", opts: linkFlagOptions{omitDWARF: optionalBool{set: true, value: true}}, want: []string{"--strip-debug"}},
		{name: "s implies w", opts: linkFlagOptions{stripSymbols: true}, want: []string{"--strip-debug"}},
		{name: "explicit w false", opts: linkFlagOptions{stripSymbols: true, omitDWARF: optionalBool{set: true}}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := dwarfStripArgs(tt.opts); !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("dwarfStripArgs(%+v) = %v, want %v", tt.opts, got, tt.want)
			}
		})
	}
}

func TestValidateDwarfStripSupport(t *testing.T) {
	w := linkFlagOptions{omitDWARF: optionalBool{set: true, value: true}}
	wFalse := linkFlagOptions{omitDWARF: optionalBool{set: true, value: false}}
	tests := []struct {
		name    string
		conf    Config
		opts    linkFlagOptions
		wantErr bool
	}{
		{name: "linux executable", conf: Config{Goos: "linux", BuildMode: BuildModeExe}, opts: w},
		{name: "darwin executable", conf: Config{Goos: "darwin", BuildMode: BuildModeExe}, opts: w, wantErr: runtime.GOOS != "darwin"},
		{name: "unsupported native OS", conf: Config{Goos: "windows", BuildMode: BuildModeExe}, opts: w, wantErr: true},
		{name: "unsupported build mode", conf: Config{Goos: "linux", BuildMode: BuildModeCShared}, opts: w, wantErr: true},
		{name: "target omit", conf: Config{Target: "wasi", Goos: "wasip1", BuildMode: BuildModeExe}, opts: w},
		{name: "target explicit DWARF", conf: Config{Target: "wasi", Goos: "wasip1", BuildMode: BuildModeExe}, opts: wFalse, wantErr: true},
		{name: "no omission", conf: Config{Goos: "windows", BuildMode: BuildModeExe}, opts: wFalse},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := validateDwarfStripSupport(&tt.conf, tt.opts)
			if (err != nil) != tt.wantErr {
				t.Fatalf("validateDwarfStripSupport() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestStripLinkedDWARFRemovesELFDebugInfo(t *testing.T) {
	if runtime.GOOS != "linux" {
		t.Skip("ELF debug-section integration test")
	}

	llvmEnv := llvm.New("")
	dir := t.TempDir()
	source := filepath.Join(dir, "main.c")
	bin := filepath.Join(dir, "app")
	if err := os.WriteFile(source, []byte("int main(void) { return 0; }\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	clang := filepath.Join(llvmEnv.BinDir(), "clang++")
	if out, err := exec.Command(clang, "-g", "-o", bin, source).CombinedOutput(); err != nil {
		t.Fatalf("compile native DWARF fixture: %v\n%s", err, out)
	}
	if !elfHasDebugInfo(t, bin) {
		t.Fatal("native fixture has no debug information before stripping")
	}

	ctx := &context{
		env:       llvmEnv,
		buildConf: &Config{Goos: "linux", Goarch: runtime.GOARCH, BuildMode: BuildModeExe},
		linkFlags: linkFlagOptions{omitDWARF: optionalBool{set: true, value: true}},
	}
	if err := stripLinkedDWARF(ctx, bin, false); err != nil {
		t.Fatal(err)
	}
	if elfHasDebugInfo(t, bin) {
		t.Fatal("debug information remains after stripping")
	}
	if out, err := exec.Command(bin).CombinedOutput(); err != nil {
		t.Fatalf("run stripped fixture: %v\n%s", err, out)
	}
}

func elfHasDebugInfo(t *testing.T, path string) bool {
	t.Helper()
	f, err := elf.Open(path)
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	for _, section := range f.Sections {
		if strings.HasPrefix(section.Name, ".debug_") || strings.HasPrefix(section.Name, ".zdebug_") {
			return true
		}
	}
	return false
}
