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
	"debug/macho"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/internal/lto"
	"github.com/xgo-dev/llgo/internal/optlevel"
	"github.com/xgo-dev/llgo/internal/packages"
	llssa "github.com/xgo-dev/llgo/ssa"
)

func TestPlanDarwinSizeSymbols(t *testing.T) {
	t.Setenv("LDFLAGS", "")
	base := Config{
		Goos:               "darwin",
		BuildMode:          BuildModeExe,
		OptLevel:           optlevel.Os,
		OmitDWARFByDefault: true,
		PCLNMode:           PCLNNone,
	}
	tests := []struct {
		name      string
		conf      Config
		native    bool
		linkArgs  []string
		wantArgs  []string
		wantStrip bool
	}{
		{name: "non-lto", conf: base, native: true, wantArgs: []string{"-Wl,-no_exported_symbols"}},
		{name: "full-lto", conf: withLTO(base, lto.Full), native: true, wantStrip: true},
		{name: "thin-lto", conf: withLTO(base, lto.Thin), native: true, wantStrip: true},
		{name: "oz", conf: withOptLevel(base, optlevel.Oz), native: true, wantArgs: []string{"-Wl,-no_exported_symbols"}},
		{name: "o2", conf: withOptLevel(base, optlevel.O2), native: true},
		{name: "cross-host", conf: base},
		{name: "named-target", conf: withTarget(base, "wasi"), native: true},
		{name: "c-shared", conf: withBuildMode(base, BuildModeCShared), native: true},
		{name: "preserve-dwarf", conf: withDWARF(base, DWARFPreserve), native: true},
		{name: "embedded-without-sites", conf: withPCLN(base, PCLNEmbedded), native: true},
		{name: "explicit-export", conf: base, native: true, linkArgs: []string{"-Wl,-exported_symbol,_entry"}},
		{name: "dynamic-lookup", conf: base, native: true, linkArgs: []string{"-Wl,-undefined,dynamic_lookup"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			ctx := &context{buildConf: &tt.conf}
			plan := planDarwinSizeSymbolsFor(ctx, nil, tt.linkArgs, tt.native)
			if !reflect.DeepEqual(plan.linkerArgs, tt.wantArgs) || plan.stripLTOLocals != tt.wantStrip {
				t.Fatalf("plan = %+v, want args=%v strip=%v", plan, tt.wantArgs, tt.wantStrip)
			}
		})
	}
}

func TestHasDarwinExportControl(t *testing.T) {
	tests := []struct {
		args []string
		want bool
	}{
		{args: []string{"-Wl,-dead_strip"}},
		{args: []string{"-Wl,-export_dynamic"}, want: true},
		{args: []string{"-Wl,-exported_symbols_list,exports.txt"}, want: true},
		{args: []string{"-Wl,-no_exported_symbols"}, want: true},
		{args: []string{"-Wl,-undefined,dynamic_lookup"}, want: true},
	}
	for _, tt := range tests {
		if got := hasDarwinExportControl(tt.args); got != tt.want {
			t.Errorf("hasDarwinExportControl(%v) = %v, want %v", tt.args, got, tt.want)
		}
	}
}

func TestPlanDarwinSizeSymbolsHonorsEnvironmentExportControl(t *testing.T) {
	t.Setenv("LDFLAGS", "-Wl,-exported_symbol,_entry")
	conf := Config{
		Goos:               "darwin",
		BuildMode:          BuildModeExe,
		OptLevel:           optlevel.Os,
		OmitDWARFByDefault: true,
		PCLNMode:           PCLNNone,
	}
	ctx := &context{buildConf: &conf}
	if plan := planDarwinSizeSymbolsFor(ctx, nil, nil, true); len(plan.linkerArgs) != 0 || plan.stripLTOLocals {
		t.Fatalf("plan with explicit environment export = %+v, want no automatic symbol compaction", plan)
	}
}

func TestMainPackageHasExports(t *testing.T) {
	prog := llssa.NewProgram(nil)
	defer prog.Dispose()
	runtimePkg := prog.NewPackage("runtime", "runtime")
	runtimePkg.SetExport("runtime.callback", "callback")
	mainPkg := prog.NewPackage("main", "main")
	pkgs := []*aPackage{
		{Package: &packages.Package{Name: "runtime", PkgPath: "runtime"}, LPkg: runtimePkg},
		{Package: &packages.Package{Name: "main", PkgPath: "main"}, LPkg: mainPkg},
	}
	if mainPackageHasExports(pkgs) {
		t.Fatal("runtime callback was treated as a user-facing executable export")
	}
	mainPkg.SetExport("Entry", "Entry")
	if !mainPackageHasExports(pkgs) {
		t.Fatal("main package C export was not detected")
	}
}

func TestDarwinSizeSymbolsIntegration(t *testing.T) {
	if runtime.GOOS != "darwin" {
		t.Skip("Mach-O symbol compaction integration test")
	}
	t.Setenv("LDFLAGS", "")
	t.Setenv(llgoFuncInfo, "1")
	tests := []struct {
		name      string
		lto       lto.Mode
		pcln      PCLNMode
		wantLocal bool
	}{
		{name: "non-lto", pcln: PCLNEmbedded, wantLocal: true},
		{name: "full-lto", lto: lto.Full, pcln: PCLNEmbedded},
		{name: "external-full-lto", lto: lto.Full, pcln: PCLNExternal},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			binaryPath := filepath.Join(t.TempDir(), "size-symbols")
			cfg := &Config{
				Mode:               ModeBuild,
				OutFile:            binaryPath,
				OptLevel:           optlevel.Os,
				LTO:                tt.lto,
				OmitDWARFByDefault: true,
				PCLNMode:           tt.pcln,
				PCLNModeSet:        true,
			}
			if _, err := Do([]string{"./testdata/ldflagsstrip"}, cfg); err != nil {
				t.Fatal(err)
			}
			cmd := exec.Command(binaryPath)
			cmd.Env = append(os.Environ(), "LLGO_FUNCINFO_DEBUG=1")
			output, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run compact binary: %v\n%s", err, output)
			}
			if !strings.Contains(string(output), "main.caller main.go true") ||
				(tt.pcln == PCLNEmbedded && !strings.Contains(string(output), "entries= prebuilt")) {
				t.Fatalf("runtime PCLN output:\n%s", output)
			}
			if output, err := exec.Command("codesign", "--verify", "--verbose=4", binaryPath).CombinedOutput(); err != nil {
				t.Fatalf("codesign verification: %v\n%s", err, output)
			}
			if tt.pcln == PCLNExternal {
				if _, err := os.Stat(pclnSidecarPath(binaryPath)); err != nil {
					t.Fatalf("external PCLN sidecar: %v", err)
				}
			}

			file, err := macho.Open(binaryPath)
			if err != nil {
				t.Fatal(err)
			}
			defer file.Close()
			if file.Symtab == nil {
				t.Fatal("Mach-O has no symbol table")
			}
			localDefined := 0
			externalDefined := 0
			for _, symbol := range file.Symtab.Syms {
				if symbol.Sect == 0 {
					continue
				}
				if symbol.Type&0x01 != 0 && symbol.Type&0x10 == 0 { // N_EXT without N_PEXT
					externalDefined++
				} else {
					localDefined++
				}
			}
			if tt.wantLocal {
				if localDefined == 0 || externalDefined != 0 {
					t.Fatalf("non-LTO defined symbols: local=%d external=%d", localDefined, externalDefined)
				}
			} else if localDefined != 0 {
				t.Fatalf("LTO retained %d local defined symbols", localDefined)
			}
		})
	}
}

func withLTO(conf Config, mode lto.Mode) Config {
	conf.LTO = mode
	return conf
}

func withOptLevel(conf Config, level optlevel.Level) Config {
	conf.OptLevel = level
	return conf
}

func withTarget(conf Config, target string) Config {
	conf.Target = target
	return conf
}

func withBuildMode(conf Config, mode BuildMode) Config {
	conf.BuildMode = mode
	return conf
}

func withDWARF(conf Config, mode DWARFMode) Config {
	conf.LinkOptions.DWARF = mode
	return conf
}

func withPCLN(conf Config, mode PCLNMode) Config {
	conf.PCLNMode = mode
	return conf
}
