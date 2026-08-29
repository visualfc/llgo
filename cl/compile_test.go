//go:build !llgo
// +build !llgo

/*
 * Copyright (c) 2024 The XGo Authors (xgo.dev). All rights reserved.
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

package cl_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/cl/cltest"
	"github.com/xgo-dev/llgo/internal/build"
	"github.com/xgo-dev/llgo/internal/buildenv"
	"github.com/xgo-dev/llgo/internal/cabi"
	"github.com/xgo-dev/llgo/internal/llgen"
	"github.com/xgo-dev/llgo/internal/lto"
	llvmenv "github.com/xgo-dev/llgo/xtool/env/llvm"
)

func testCompile(t *testing.T, src, expected string) {
	t.Helper()
	cltest.TestCompileEx(t, src, "foo.go", expected, false)
}

func requireEmbedTest(t *testing.T) {
	t.Helper()
	if os.Getenv("LLGO_EMBED_TESTS") != "1" {
		t.Skip("Skipping embedded emulator tests; set LLGO_EMBED_TESTS=1 to run")
	}
}

type embedTestSuite struct {
	name   string
	relDir string
}

type embedTargetConfig struct {
	target      string
	ignoreByDir map[string][]string
}

var embedTestSuites = []embedTestSuite{
	{name: "testgo", relDir: "./_testgo"},
	{name: "testlibc", relDir: "./_testlibc"},
	{name: "testrt", relDir: "./_testrt"},
	{name: "testdata", relDir: "./_testdata"},
}

var embedTargetConfigs = []embedTargetConfig{
	{
		target: "esp32c3-basic",
		ignoreByDir: map[string][]string{
			"./_testgo": {
				"./_testgo/abimethod",   // llgo panic: unsatisfied import internal/runtime/sys
				"./_testgo/cgobasic",    // fast fail: build constraints exclude all Go files (cgo)
				"./_testgo/cgocfiles",   // fast fail: build constraints exclude all Go files (cgo)
				"./_testgo/cgodefer",    // fast fail: build constraints exclude all Go files (cgo)
				"./_testgo/cgofull",     // fast fail: build constraints exclude all Go files (cgo)
				"./_testgo/cgomacro",    // fast fail: build constraints exclude all Go files (cgo)
				"./_testgo/cgopython",   // fast fail: build constraints exclude all Go files (cgo)
				"./_testgo/chan",        // timeout: emulator did not auto-exit
				"./_testgo/complitnil",  // baremetal terminates on the nil-pointer panic before deferred recovery
				"./_testgo/cursor",      // panic: internal/bytealg: selected .s files require plan9asm translation
				"./_testgo/defer4",      // unexpected output: got "fatal error", expected "recover: panic message"
				"./_testgo/goexit",      // llgo panic: unsatisfied import internal/runtime/sys
				"./_testgo/indexerr",    // unexpected output: len(dst)=12, len(src)=0 (got "fatal error")
				"./_testgo/makeslice",   // unexpected output: len(dst)=23, len(src)=0 (got "fatal error\\nmust error")
				"./_testgo/mapindirect", // ld.lld: error: undefined symbol: __atomic_fetch_or_4
				"./_testgo/reflect",     // llgo panic: unsatisfied import internal/runtime/sys
				"./_testgo/reflectconv", // llgo panic: unsatisfied import internal/sync
				"./_testgo/reflectfn",   // llgo panic: unsatisfied import internal/runtime/sys
				"./_testgo/reflectmkfn", // llgo panic: unsatisfied import internal/runtime/sys
				"./_testgo/rewrite",     // llgo panic: unsatisfied import internal/sync
				"./_testgo/runextest",   // package-selection fixture owned by internal/build
				"./_testgo/runtest",     // package-selection fixture owned by internal/build
				"./_testgo/select",      // timeout: emulator did not auto-exit
				"./_testgo/selects",     // timeout: emulator did not auto-exit
				"./_testgo/sigsegv",     // unexpected output: got "0/main", expected recover nil-pointer message
				// Baremetal terminates after an outermost panic is recovered.
				"./_testgo/nesteddeferpanic",
			},
			"./_testlibc": {
				"./_testlibc/argv",    // timeout: emulator panic (Load access fault), no auto-exit
				"./_testlibc/atomic",  // link error: ld.lld: error: undefined symbol: __atomic_store
				"./_testlibc/complex", // link error: ld.lld: error: undefined symbol: cabsf
				"./_testlibc/cppabi",  // C++ standard headers are unavailable in the baremetal sysroot
				"./_testlibc/once",    // fast fail: build constraints exclude all Go files (pthread/sync)
				"./_testlibc/setjmp",  // link error: ld.lld: error: undefined symbol: stderr
			},
			"./_testrt": {
				"./_testrt/asmfull",     // compile/asm error: unrecognized instruction mnemonic
				"./_testrt/fprintf",     // link error: ld.lld: error: undefined symbol: __stderrp
				"./_testrt/linkname",    // unexpected output: line order mismatch ("hello" appears first)
				"./_testrt/makemap",     // link error: ld.lld: error: undefined symbol: __atomic_fetch_or_4
				"./_testrt/strlen",      // fast fail: build constraints exclude all Go files
				"./_testrt/struct",      // fast fail: build constraints exclude all Go files
				"./_testrt/tpfunc",      // unexpected output: type size mismatch (got 8 4 4, expected 16 8 8)
				"./_testrt/typalias",    // fast fail: build constraints exclude all Go files
				"./_testrt/unreachable", // timeout: emulator panic (Instruction access fault), no auto-exit

				"./_testrt/reflectclosureenv", // baseline embedded runtime cannot build this reflect path
				"./_testrt/ptrtothislazy",     // baseline embedded runtime cannot build this reflect path
				"./_testrt/ptrtothislazynew",  // baseline embedded runtime cannot build this reflect path
			},
			"./_testdata": {
				"./_testdata/debug", // llgo panic: unsatisfied import internal/runtime/sys
			},
		},
	},
	{
		target: "esp32",
		ignoreByDir: map[string][]string{
			"./_testgo": {
				"./_testgo/abimethod",   // panic: internal/bytealg selected .s files require plan9asm translation
				"./_testgo/alias",       // unexpected output
				"./_testgo/cgodefer",    // panic: cannot build SSA for packages
				"./_testgo/cgopython",   // panic: cannot build SSA for packages
				"./_testgo/complitnil",  // baremetal terminates on the nil-pointer panic before deferred recovery
				"./_testgo/cursor",      // panic: internal/bytealg: selected .s files require plan9asm translation
				"./_testgo/defer4",      // runtime output: fatal error
				"./_testgo/indexerr",    // runtime output: fatal error
				"./_testgo/invoke",      // unexpected output
				"./_testgo/makeslice",   // runtime output: fatal error
				"./_testgo/mapindirect", // fatal error: error in backend: Incomplete scavenging after 2nd pass
				"./_testgo/multiret",    // unexpected output
				"./_testgo/runextest",   // package-selection fixture owned by internal/build
				"./_testgo/runtest",     // package-selection fixture owned by internal/build
				"./_testgo/select",      // timeout: emulator did not auto-exit
				"./_testgo/sigsegv",     // unexpected output
				"./_testgo/struczero",   // timeout: emulator did not auto-exit
				// Baremetal terminates after an outermost panic is recovered.
				"./_testgo/nesteddeferpanic",
			},
			"./_testlibc": {
				"./_testlibc/atomic", // unexpected output
				"./_testlibc/cppabi", // C++ standard headers are unavailable in the baremetal sysroot
				"./_testlibc/once",   // panic: cannot build SSA for packages
				"./_testlibc/setjmp", // link error: ld.lld undefined symbol stderr
			},
			"./_testrt": {
				"./_testrt/asmfull",  // unexpected output
				"./_testrt/cast",     // timeout: emulator did not auto-exit
				"./_testrt/complex",  // unexpected output
				"./_testrt/fprintf",  // link error: ld.lld undefined symbol __stderrp
				"./_testrt/linkname", // unexpected output
				"./_testrt/strlen",   // panic: runtime index out of range
				"./_testrt/struct",   // panic: runtime index out of range
				"./_testrt/tpfunc",   // unexpected output
				"./_testrt/typalias", // panic: runtime index out of range

				"./_testrt/reflectclosureenv", // baseline embedded runtime cannot build this reflect path
				"./_testrt/ptrtothislazy",     // baseline embedded runtime cannot build this reflect path
				"./_testrt/ptrtothislazynew",  // baseline embedded runtime cannot build this reflect path
			},
			"./_testdata": {
				"./_testdata/cpkgimp", // unexpected output
			},
		},
	},
}

func runEmbedTargetSuite(t *testing.T, target, relDir string, ignore []string) {
	t.Helper()
	conf := build.NewDefaultConf(build.ModeRun)
	conf.Target = target
	conf.Emulator = true
	cltest.RunAndTestFromDir(t, "", relDir, ignore,
		cltest.WithRunConfig(conf),
		cltest.WithOutputFilter(cltest.FilterEmulatorOutput),
		cltest.WithIRCheck(false),
	)
}

func TestRunAndTestFromTestgo(t *testing.T) {
	// Package-selection fixtures remain in place for internal/build, but none
	// of the compiler runners treat them as lowering owners.
	ignore := []string{
		"./_testgo/runextest",
		"./_testgo/runtest",
	}
	cltest.RunAndTestFromDir(t, "", "./_testgo", ignore)
}

func TestRunAndTestFromTestmeta(t *testing.T) {
	conf := build.NewDefaultConf(build.ModeRun)
	conf.CollectPackageMeta = true
	cltest.RunAndTestFromDir(t, "", "./_testmeta", nil,
		cltest.WithRunConfig(conf),
		cltest.WithOutputCheck(false),
		cltest.WithIRCheck(false),
		cltest.WithMetaCheck(true),
	)
}

func TestRunAndTestFromTestlto(t *testing.T) {
	conf := build.NewDefaultConf(build.ModeRun)
	conf.LTO = lto.Full
	ignore := []string{
		"./_testlto/globaldce_static_itab_devirt",
		"./_testlto/globaldce_static_itab_partial_root",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_concat",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_global",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_global_slice",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_loop",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_param",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_range_literal",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_slice",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_string_abi",
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_switch",
	}
	if !buildenv.Dev {
		ignore = append(ignore,
			"./_testlto/globaldce_abitype_fakeuse",
			"./_testlto/globaldce_interface_matrix",
			"./_testlto/globaldce_interface_slots",
			"./_testlto/globaldce_reflect_method",
			"./_testlto/globaldce_reflect_type_method",
			"./_testlto/globaldce_reflect_type_method_by_name",
			"./_testlto/globaldce_reflect_type_method_metadata_only",
			"./_testlto/globaldce_reflect_value_method",
			"./_testlto/globaldce_typeid_dce",
			"./_testlto/globaldce_unexported_method_identity",
			"./_testlto/anonymous_alias",
		)
	}
	cltest.RunAndTestFromDir(t, "", "./_testlto", ignore, cltest.WithRunConfig(conf))
}

var testltoSymbolChecks = []string{
	"globaldce_interface_matrix",
	"globaldce_interface_slots",
	"globaldce_reflect_method",
	"globaldce_reflect_type_method_by_name",
	"globaldce_reflect_value_method",
	"globaldce_typeid_dce",
	"globaldce_unexported_method_identity",
}

var testltoLTOPluginTests = []string{
	"globaldce_static_itab_devirt",
	"globaldce_static_itab_partial_root",
	"globaldce_reflect_type_method_metadata_only",
	"globaldce_reflect_method_by_name_ltoplugin",
	"globaldce_reflect_method_by_name_ltoplugin_concat",
	"globaldce_reflect_method_by_name_ltoplugin_global",
	"globaldce_reflect_method_by_name_ltoplugin_global_slice",
	"globaldce_reflect_method_by_name_ltoplugin_loop",
	"globaldce_reflect_method_by_name_ltoplugin_param",
	"globaldce_reflect_method_by_name_ltoplugin_range_literal",
	"globaldce_reflect_method_by_name_ltoplugin_slice",
	"globaldce_reflect_method_by_name_ltoplugin_string_abi",
	"globaldce_reflect_method_by_name_ltoplugin_switch",
}

func TestBuildAndCheckSymbolsFromTestlto(t *testing.T) {
	if !buildenv.Dev {
		t.Skip("globaldce symbol checks require dev build")
	}
	conf := build.NewDefaultConf(build.ModeBuild)
	conf.LTO = lto.Full
	// Linux exports main.* when PCLN is enabled so runtime funcinfo can resolve
	// symbols. Disable that retention here so the final symbol table measures
	// GlobalDCE rather than the executable's dynamic-export policy.
	conf.PCLNMode = build.PCLNNone
	cltest.BuildAndCheckSymbolsFromDir(t, "", "./_testlto", testltoSymbolChecks, cltest.WithRunConfig(conf))
}

var testdropSymbolChecks = []string{
	"c_export_callback",
	"direct_func",
	"direct_method",
	"exported_method_crosspkg",
	"generic_interface_crosspkg",
	"generic_interface_func_crosspkg",
	"iface_flow_crosspkg",
	"interface_demand_fixedpoint",
	"interface_match",
	"interface_slot",
	"promoted_method_wrapper",
	"reflect_dynamic_iface_crosspkg",
	"reflect_field_addr_iface",
	"reflect_method_result",
	"reflect_named_method",
	"source64_crosspkg",
	"unexported_method_identity",
}

func TestBuildAndCheckSymbolsFromTestdrop(t *testing.T) {
	if !buildenv.Dev {
		t.Skip("deadcode drop symbol checks require dev build")
	}
	conf := build.NewDefaultConf(build.ModeBuild)
	conf.DeadcodeDrop = true
	// Linux exports main.* when PCLN is enabled, which retains otherwise-dead
	// methods. Disable that retention so the symbol table measures method DCE.
	conf.PCLNMode = build.PCLNNone
	cltest.BuildAndCheckSymbolsFromDir(t, "", "./_testdrop", testdropSymbolChecks,
		cltest.WithRunConfig(conf),
		cltest.WithOutputCheck(true),
	)
}

func testltoLTOPluginConf(t *testing.T, mode build.Mode) *build.Config {
	t.Helper()
	if !buildenv.Dev {
		t.Skip("globaldce plugin tests require dev build")
	}
	plugin := os.Getenv("LLGO_LTO_PLUGIN")
	if plugin == "" {
		t.Skip("set LLGO_LTO_PLUGIN to the built LLGOLTOPlugin shared library")
	}
	conf := build.NewDefaultConf(mode)
	conf.LTO = lto.Full
	conf.LTOPlugin = lto.PassPlugin{Path: plugin}
	return conf
}

func TestRunAndTestFromTestltoLTOPlugin(t *testing.T) {
	conf := testltoLTOPluginConf(t, build.ModeRun)
	cltest.RunAndTestFromDir(t, "ltoplugin", "./_testlto", nil,
		cltest.WithRunConfig(conf),
		cltest.WithIRCheck(false),
	)
}

func TestBuildAndCheckSymbolsFromTestltoLTOPlugin(t *testing.T) {
	buildConf := testltoLTOPluginConf(t, build.ModeBuild)
	// See TestBuildAndCheckSymbolsFromTestlto: dynamic main.* exports retain
	// otherwise-dead symbols on Linux and would mask the plugin's DCE result.
	buildConf.PCLNMode = build.PCLNNone
	cltest.BuildAndCheckSymbolsFromDir(t, "", "./_testlto", testltoLTOPluginTests,
		cltest.WithRunConfig(buildConf),
	)
}

func runTestltoLTOPluginAggregateABI(t *testing.T, fixture string) string {
	t.Helper()
	conf := testltoLTOPluginConf(t, build.ModeGen)
	// Apply a fixed LP64 ABI below so the aggregate form is tested on every host.
	conf.AbiMode = cabi.ModeNone
	plugin := conf.LTOPlugin.Path
	pkgs, err := build.Do([]string{fixture}, conf)
	if err != nil {
		t.Fatalf("generate aggregate string module: %v", err)
	}
	if len(pkgs) != 1 {
		t.Fatalf("generate aggregate string module: got %d packages", len(pkgs))
	}
	cabi.NewTransformer(pkgs[0].LPkg.Prog, "arm64-unknown-linux", "", cabi.ModeAllFunc, true).
		TransformModule(pkgs[0].PkgPath, pkgs[0].LPkg.Module())
	aggregateIR := pkgs[0].LPkg.String()
	pkgs[0].LPkg.Prog.Dispose()
	if !strings.Contains(aggregateIR, `runtime.String" "llgo.reflect.methodbyname.name"`) {
		t.Fatalf("MethodByName string argument was not captured in aggregate form:\n%s", aggregateIR)
	}

	opt := filepath.Join(llvmenv.New("").BinDir(), "opt")
	cmd := exec.Command(opt, "-load-pass-plugin="+plugin,
		"-passes=llgo-lto-pre-globaldce", "-S", "-o", "-")
	cmd.Stdin = strings.NewReader(aggregateIR)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run LTO plugin for aggregate ABI: %v\n%s", err, out)
	}
	return string(out)
}

func TestBuildAndCheckSymbolsFromTestltoLTOPluginAggregateABI(t *testing.T) {
	result := runTestltoLTOPluginAggregateABI(t,
		"./_testlto/globaldce_reflect_method_by_name_ltoplugin_string_abi")
	for _, name := range []string{"Direct", "Concat", "Slice", "Forward"} {
		marker := `metadata !"go.method.value.reflect.` + name + `"`
		if !strings.Contains(result, marker) {
			t.Fatalf("aggregate ABI output missing %s\n%s", marker, result)
		}
	}
	if strings.Contains(result, `metadata !"go.method.value.reflect"`) {
		t.Fatalf("aggregate ABI output retained the generic value marker\n%s", result)
	}

	// The helper above still verifies that the dynamic name survives aggregate
	// ABI lowering. Since this module discards the returned Method, however, it
	// must not synthesize a generic Func-capability marker that retains every
	// matching method body.
	unknownResult := runTestltoLTOPluginAggregateABI(t,
		"./_testlto/_globaldce_reflect_method_by_name_ltoplugin_string_abi_unknown")
	if strings.Contains(unknownResult, `metadata !"go.method.type.reflect"`) {
		t.Fatalf("aggregate ABI output retained a generic type Func marker\n%s", unknownResult)
	}
}

func TestBuildAndCheckSymbolsFromTestltoLTOPluginPartialStaticItabDevirt(t *testing.T) {
	conf := testltoLTOPluginConf(t, build.ModeGen)
	const input = `
target datalayout = "e-p:64:64-i64:64-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@interface.I = constant i8 0
@type.A = constant i8 0
@_llgo_itab$A = weak_odr constant { ptr, ptr, i32, [1 x ptr] } { ptr @interface.I, ptr @type.A, i32 0, [1 x ptr] [ptr @A.M] }, !llgo.static.itab.slot !0

declare ptr @runtime.NewItab(ptr, ptr)
declare { ptr, i1 } @llvm.type.checked.load(ptr, i32, metadata)
declare void @sink(ptr)

define void @A.M(ptr %recv) {
entry:
	ret void
}

define void @test(ptr %dynamic.itab) {
entry:
	%static.itab = call ptr @runtime.NewItab(ptr @interface.I, ptr @type.A)
	%static.load = call { ptr, i1 } @llvm.type.checked.load(ptr %static.itab, i32 24, metadata !"go.method.M:func()")
	%static.fn = extractvalue { ptr, i1 } %static.load, 0
	call void @sink(ptr %static.fn)
	%dynamic.load = call { ptr, i1 } @llvm.type.checked.load(ptr %dynamic.itab, i32 24, metadata !"go.method.M:func()")
	%dynamic.fn = extractvalue { ptr, i1 } %dynamic.load, 0
	call void @sink(ptr %dynamic.fn)
	ret void
}

!0 = !{i64 24, !"go.method.M:func()"}
`
	opt := filepath.Join(llvmenv.New("").BinDir(), "opt")
	cmd := exec.Command(opt, "-load-pass-plugin="+conf.LTOPlugin.Path,
		"-passes=llgo-lto-pre-globaldce", "-S", "-o", "-")
	cmd.Stdin = strings.NewReader(input)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("run LTO plugin for partial static itab devirtualization: %v\n%s", err, out)
	}
	result := string(out)
	if got := strings.Count(result, "call { ptr, i1 } @llvm.type.checked.load"); got != 1 {
		t.Fatalf("got %d checked loads after partial devirtualization, want 1\n%s", got, result)
	}
	if !strings.Contains(result, "ptr @A.M") {
		t.Fatalf("static NewItab call was not resolved to A.M\n%s", result)
	}
	if !strings.Contains(result,
		`@llvm.type.checked.load(ptr %dynamic.itab, i32 24, metadata !"go.method.M:func()")`) {
		t.Fatalf("dynamic checked load was not preserved\n%s", result)
	}
}

func TestFilterEmulatorOutput(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name: "ESP32C3 output",
			input: `Adding SPI flash device
ESP-ROM:esp32c3-api1-20210207
Build:Feb  7 2021
rst:0x1 (POWERON),boot:0x8 (SPI_FAST_FLASH_BOOT)
SPIWP:0xee
mode:DIO, clock div:1
load:0x3fc855b0,len:0xfc
load:0x3fc856ac,len:0x4
load:0x3fc856b0,len:0x44
load:0x40380000,len:0x1548
load:0x40381548,len:0x68
entry 0x40380000
Hello World!
`,
			expected: `Hello World!
`,
		},
		{
			name: "ESP32 output",
			input: `Adding SPI flash device
ESP-ROM:esp32-xxxx
entry 0x40080000
Hello World!
`,
			expected: `Hello World!
`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := cltest.FilterEmulatorOutput(tt.input)
			if got != tt.expected {
				t.Fatalf("filterEmulatorOutput() = %q, want %q", got, tt.expected)
			}
		})
	}
}

func TestRunEmbedEmulator(t *testing.T) {
	requireEmbedTest(t)
	for _, targetConf := range embedTargetConfigs {
		targetConf := targetConf
		t.Run(targetConf.target, func(t *testing.T) {
			for _, suite := range embedTestSuites {
				suite := suite
				t.Run(suite.name, func(t *testing.T) {
					runEmbedTargetSuite(t, targetConf.target, suite.relDir, targetConf.ignoreByDir[suite.relDir])
				})
			}
		})
	}
}

func TestRunFromTestgoSelectAllowsKnownInterleavings(t *testing.T) {
	output, err := cltest.RunAndCapture("./_testgo/select", "")
	if err != nil {
		t.Fatalf("run failed: %v\noutput: %s", err, string(output))
	}
	lines := selectOutputLines(string(output))
	if !validSelectOutputLines(lines) {
		t.Fatalf("unexpected select output lines %q from:\n%s", lines, output)
	}
}

func validSelectOutputLines(lines []string) bool {
	sendCount, recvCount := 0, 0
	seenCh1, seenCh2 := false, false
	for _, line := range lines {
		switch line {
		case "100", "200":
			sendCount++
			if sendCount > 1 {
				return false
			}
		case "ch1":
			if seenCh1 {
				return false
			}
			seenCh1 = true
			recvCount++
		case "ch2":
			if seenCh2 {
				return false
			}
			seenCh2 = true
			recvCount++
		case "exit":
			recvCount++
		default:
			return false
		}
	}
	return recvCount == 2
}

func TestValidSelectOutputLines(t *testing.T) {
	tests := []struct {
		name  string
		lines []string
		valid bool
	}{
		{name: "sender exits before print", lines: []string{"ch1", "ch2"}, valid: true},
		{name: "both receives default", lines: []string{"exit", "exit"}, valid: true},
		{name: "send prints first", lines: []string{"100", "ch1", "ch2"}, valid: true},
		{name: "send print is interleaved", lines: []string{"ch1", "200", "exit"}, valid: true},
		{name: "duplicate receive", lines: []string{"ch1", "ch1"}},
		{name: "missing receive", lines: []string{"100", "ch1"}},
		{name: "extra receive", lines: []string{"ch1", "ch2", "exit"}},
		{name: "multiple sends", lines: []string{"100", "200", "ch1", "ch2"}},
		{name: "unknown output", lines: []string{"100", "ch1", "unknown"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := validSelectOutputLines(tt.lines); got != tt.valid {
				t.Fatalf("validSelectOutputLines(%q) = %v, want %v", tt.lines, got, tt.valid)
			}
		})
	}
}

func selectOutputLines(output string) []string {
	// Builtin print operations from different native goroutine threads can be
	// interleaved: an integer may be split around another token, and two string
	// tokens may share one physical line. Extract every complete logical token
	// in order and leave incomplete integer fragments unclassified.
	tokens := [...]string{"100", "200", "ch1", "ch2", "exit"}
	var lines []string
	for _, line := range strings.Split(output, "\n") {
		line = strings.TrimSpace(line)
		for len(line) != 0 {
			index := len(line)
			token := ""
			for _, candidate := range tokens {
				if candidateIndex := strings.Index(line, candidate); candidateIndex >= 0 && candidateIndex < index {
					index = candidateIndex
					token = candidate
				}
			}
			if token == "" {
				break
			}
			lines = append(lines, token)
			line = line[index+len(token):]
		}
	}
	return lines
}

func TestSelectOutputLinesAllowsConcurrentPrints(t *testing.T) {
	tests := []struct {
		name   string
		output string
		want   string
	}{
		{
			name:   "split integer print",
			output: "1exit\nexit\n00\n",
			want:   "exit exit",
		},
		{
			name:   "coalesced string prints",
			output: "ch1exit\n",
			want:   "ch1 exit",
		},
		{
			name:   "coalesced integer and string prints",
			output: "100ch2\n",
			want:   "100 ch2",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := strings.Join(selectOutputLines(tt.output), " "); got != tt.want {
				t.Fatalf("selectOutputLines() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestRunAndTestFromTestpy(t *testing.T) {
	cltest.RunAndTestFromDir(t, "", "./_testpy", nil)
}

func TestRunAndTestFromTestpyDWARF(t *testing.T) {
	conf := build.NewDefaultConf(build.ModeRun)
	conf.LinkOptions.DWARF = build.DWARFPreserve
	cltest.RunAndTestFromDir(t, "", "./_testpy", nil,
		cltest.WithRunConfig(conf), cltest.WithIRCheck(false))
}

func TestRunAndTestFromTestlibgo(t *testing.T) {
	cltest.RunAndTestFromDir(t, "", "./_testlibgo", nil)
}

func TestRunAndTestFromTestlibc(t *testing.T) {
	var ignore []string
	if runtime.GOOS == "windows" {
		ignore = []string{
			"./_testlibc/once", // POSIX pthread_once has no Windows ABI counterpart.
		}
	}
	cltest.RunAndTestFromDir(t, "", "./_testlibc", ignore)
}

func TestRunAndTestFromTestrt(t *testing.T) {
	var ignore []string
	if runtime.GOOS == "linux" {
		ignore = []string{
			"./_testrt/asmfull", // Output is macOS-specific.
			"./_testrt/fprintf", // Linux uses different stderr symbol (no __stderrp).
		}
	}
	cltest.RunAndTestFromDir(t, "", "./_testrt", ignore)
}

func TestRunAndTestFromTestdata(t *testing.T) {
	cltest.RunAndTestFromDir(t, "", "./_testdata", nil)
}

func TestCgofullGeneratesC2func(t *testing.T) {
	ir := llgen.GenFrom("./_testgo/cgofull")
	if !strings.Contains(ir, "_C2func_test_structs") {
		t.Fatal("missing _C2func_test_structs in cgofull IR")
	}
	if !strings.Contains(ir, "cliteErrno") {
		t.Fatal("missing cliteErrno call in cgofull IR")
	}
}

func TestGoPkgMath(t *testing.T) {
	conf := build.NewDefaultConf(build.ModeInstall)
	_, err := build.Do([]string{"math"}, conf)
	if err != nil {
		t.Fatal(err)
	}
}

func TestVar(t *testing.T) {
	testCompile(t, `package foo

var a int
`, `; ModuleID = 'foo'
source_filename = "foo"

@foo.a = global i64 0, align 8
@"foo.init$guard" = global i1 false, align 1

; Function Attrs: null_pointer_is_valid
define void @foo.init() #0 {
_llgo_0:
  %0 = load i1, ptr @"foo.init$guard", align 1
  br i1 %0, label %_llgo_2, label %_llgo_1

_llgo_1:                                          ; preds = %_llgo_0
  store i1 true, ptr @"foo.init$guard", align 1
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
  ret void
}

attributes #0 = { null_pointer_is_valid "frame-pointer"="non-leaf" }
`)
}

func TestBasicFunc(t *testing.T) {
	testCompile(t, `package foo

func fn(a int, b float64) int {
	return 1
}
`, `; ModuleID = 'foo'
source_filename = "foo"

@"foo.init$guard" = global i1 false, align 1

; Function Attrs: null_pointer_is_valid
define i64 @foo.fn(i64 %0, double %1) #0 {
_llgo_0:
  ret i64 1
}

; Function Attrs: null_pointer_is_valid
define void @foo.init() #0 {
_llgo_0:
  %0 = load i1, ptr @"foo.init$guard", align 1
  br i1 %0, label %_llgo_2, label %_llgo_1

_llgo_1:                                          ; preds = %_llgo_0
  store i1 true, ptr @"foo.init$guard", align 1
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
  ret void
}

attributes #0 = { null_pointer_is_valid "frame-pointer"="non-leaf" }
`)
}

func TestIntrinsicBoolToUint8(t *testing.T) {
	testCompile(t, `package foo

import _ "unsafe"

//go:linkname boolToUint8 llgo.boolToUint8
func boolToUint8(b bool) uint8

func use(b bool) uint8 {
	return boolToUint8(b)
}
`, `; ModuleID = 'foo'
source_filename = "foo"

@"foo.init$guard" = global i1 false, align 1

; Function Attrs: null_pointer_is_valid
define void @foo.init() #0 {
_llgo_0:
  %0 = load i1, ptr @"foo.init$guard", align 1
  br i1 %0, label %_llgo_2, label %_llgo_1

_llgo_1:                                          ; preds = %_llgo_0
  store i1 true, ptr @"foo.init$guard", align 1
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
  ret void
}

; Function Attrs: null_pointer_is_valid
define i8 @foo.use(i1 %0) #0 {
_llgo_0:
  %1 = select i1 %0, i8 1, i8 0
  ret i8 %1
}

attributes #0 = { null_pointer_is_valid "frame-pointer"="non-leaf" }
`)
}
