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
	"testing"

	llssa "github.com/xgo-dev/llgo/ssa"
	"golang.org/x/tools/go/packages"
	"golang.org/x/tools/go/ssa"
)

func TestProgramUsesLibffi(t *testing.T) {
	if programUsesLibffi(nil) {
		t.Fatal("nil context may not require libffi")
	}
	if isLibffiCall(nil, libffiOptions{}) {
		t.Fatal("nil call may not require libffi")
	}
	if programHasSetFinalizerPtr(nil) {
		t.Fatal("nil program may not provide SetFinalizerPtr")
	}
	tests := []struct {
		name string
		src  string
		opts libffiOptions
		want bool
	}{
		{"no reflection", `package p; func f() int { return 1 }`, libffiOptions{}, false},
		{"metadata only", `package p; import "reflect"; func f() reflect.Type { return reflect.TypeOf(1) }`, libffiOptions{}, false},
		{"sequence", `package p; import "reflect"; func f(v reflect.Value) { _ = v.Seq() }`, libffiOptions{}, false},
		{"sequence two", `package p; import "reflect"; func f(v reflect.Value) { _ = v.Seq2() }`, libffiOptions{}, false},
		{"value call", `package p; import "reflect"; func f(v reflect.Value) { v.Call(nil) }`, libffiOptions{}, true},
		{"call slice", `package p; import "reflect"; func f(v reflect.Value) { v.CallSlice(nil) }`, libffiOptions{}, true},
		{"make func", `package p; import "reflect"; func f(t reflect.Type, fn func([]reflect.Value) []reflect.Value) { reflect.MakeFunc(t, fn) }`, libffiOptions{}, true},
		{"named finalizer", `package p; import "runtime"; type T int; func fin(*T) {}; func f(p *T) { runtime.SetFinalizer(p, fin) }`, libffiOptions{setFinalizerPtr: true}, false},
		{"nil finalizer", `package p; import "runtime"; type T int; func f(p *T) { runtime.SetFinalizer(p, nil) }`, libffiOptions{setFinalizerPtr: true}, false},
		{"closure finalizer", `package p; import "runtime"; type T int; func f(p *T) { n := 1; runtime.SetFinalizer(p, func(*T) { _ = n }) }`, libffiOptions{setFinalizerPtr: true}, true},
		{"finalizer without ptr", `package p; import "runtime"; type T int; func f(p *T) { n := 1; runtime.SetFinalizer(p, func(*T) { _ = n }) }`, libffiOptions{}, false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			pkg := buildWasmReflectTestProgram(t, test.src)
			if got := analyzeProgramUse(pkg.Prog, nil).usesLibffi(test.opts); got != test.want {
				t.Fatalf("usesLibffi() = %v, want %v", got, test.want)
			}
		})
	}
}

func TestProgramUsesLibffiReachability(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want bool
	}{
		{"dead call", `package main; import "reflect"; func dead(v reflect.Value) { v.Call(nil) }; func main() {}`, false},
		{"reachable call", `package main; import "reflect"; func live(v reflect.Value) { v.Call(nil) }; func main() { live(reflect.Value{}) }`, true},
		{"function value call", `package main; import "reflect"; var call = reflect.Value.Call; func main() { call(reflect.Value{}, nil) }`, true},
		{"bound method call", `package main; import "reflect"; func main() { call := reflect.Value{}.Call; call(nil) }`, true},
		{"function value make func", `package main; import "reflect"; var makeFunc = reflect.MakeFunc; func main() { makeFunc(reflect.TypeOf(func() {}), func([]reflect.Value) []reflect.Value { return nil }) }`, true},
		{"higher-order make func", `package main; import "reflect"; func invoke(makeFunc func(reflect.Type, func([]reflect.Value) []reflect.Value) reflect.Value) { makeFunc(reflect.TypeOf(func() {}), func([]reflect.Value) []reflect.Value { return nil }) }; func main() { invoke(reflect.MakeFunc) }`, true},
		{"interface call", `package main; import "reflect"; type caller interface { Call([]reflect.Value) []reflect.Value }; func main() { var call caller = reflect.Value{}; call.Call(nil) }`, true},
		{"unrelated reflect bound method", `package main; import "reflect"; func main() { typ := reflect.TypeOf(0); name := typ.String; _ = name() }`, false},
		{"dead sequence", `package main; import "reflect"; func dead(v reflect.Value) { _ = v.Seq() }; func main() {}`, false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			pkg := buildWasmReflectTestProgram(t, test.src)
			roots := []*ssa.Function{pkg.Func("init"), pkg.Func("main")}
			if got := analyzeProgramUse(pkg.Prog, roots).usesLibffi(libffiOptions{}); got != test.want {
				t.Fatalf("reachable usesLibffi() = %v, want %v", got, test.want)
			}
		})
	}
}

func TestLibffiProgramUseSharesExecutableAnalysis(t *testing.T) {
	pkg := buildWasmReflectTestProgram(t, `package main
import "reflect"
func main() { reflect.ValueOf(func() {}).Call(nil) }
`)
	target := &llssa.Target{GOOS: "wasip1", GOARCH: "wasm", WasmProvider: "wasi"}
	prog := llssa.NewProgram(target)
	defer prog.Dispose()
	ctx := &context{
		prog:      prog,
		progSSA:   pkg.Prog,
		initial:   []*packages.Package{{Types: pkg.Pkg}},
		buildConf: &Config{BuildMode: BuildModeExe},
		mode:      ModeBuild,
	}
	configureWasmReflectBridges(ctx)
	analysis := ctx.programUse
	if analysis == nil {
		t.Fatal("WASI configuration did not analyze the program")
	}
	if libffiProgramUse(ctx) != analysis {
		t.Fatal("CheckFFI executable scan used a different program analysis")
	}
	if !programUsesLibffi(ctx) {
		t.Fatal("reachable reflect.Call did not require libffi")
	}
}

func TestLibffiUseExecutableRoots(t *testing.T) {
	pkg := buildWasmReflectTestProgram(t, `package main; func main() {}`)
	ctx := &context{
		progSSA:   pkg.Prog,
		initial:   []*packages.Package{{Types: pkg.Pkg}},
		buildConf: &Config{BuildMode: BuildModeExe},
		mode:      ModeBuild,
	}
	if !libffiUseExecutableRoots(ctx) {
		t.Fatal("main executable should use init/main roots")
	}
	ctx.mode = ModeGen
	if libffiUseExecutableRoots(ctx) {
		t.Fatal("package generation should scan every function")
	}
	ctx.mode = ModeBuild
	ctx.buildConf.BuildMode = BuildModeCArchive
	if libffiUseExecutableRoots(ctx) {
		t.Fatal("c-archive should scan every function")
	}
	if libffiUseExecutableRoots(nil) {
		t.Fatal("nil context should not use executable roots")
	}
	if libffiUseExecutableRoots(&context{}) {
		t.Fatal("empty context should not use executable roots")
	}
}

func TestLibffiNameHelpers(t *testing.T) {
	if isLibffiReflectName("Seq") || isLibffiReflectName("Seq2") || !isLibffiReflectName("MakeFunc") {
		t.Fatal("libffi reflect names include Call/CallSlice/MakeFunc only")
	}
	if skipLibffiCallSite("fmt") || !skipLibffiCallSite("reflect") || !skipLibffiCallSite("runtime") {
		t.Fatal("libffi call sites skip reflect and runtime")
	}
	if setFinalizerCallNeedsFFI(nil) != true {
		t.Fatal("nil SetFinalizer call must keep libffi")
	}
}
