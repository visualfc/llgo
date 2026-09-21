//go:build !llgo

package build

import (
	"bytes"
	"encoding/json"
	"go/types"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"testing"

	"github.com/xgo-dev/llgo/internal/packages"
	llssa "github.com/xgo-dev/llgo/ssa"
)

func TestGroupInitialBuilds(t *testing.T) {
	prog := llssa.NewProgram(&llssa.Target{GOARCH: "amd64"})
	defer prog.Dispose()
	plain := &packages.Package{Types: types.NewPackage("example.com/plain", "main")}
	local := &packages.Package{Types: types.NewPackage("example.com/local", "main")}
	other := &packages.Package{Types: types.NewPackage("example.com/other", "main")}
	prog.DeclareLocality(local.Types, "state", llssa.LocalityInfo{Locality: llssa.GoroutineLocal})
	ctx := &context{prog: prog, initial: []*packages.Package{plain, local, other}, mode: ModeBuild}
	groups := groupInitialBuilds(ctx, nil)
	if len(groups) != 2 || groups[0].features.localContext || !groups[1].features.localContext ||
		!slices.Equal(groups[0].pkgs, []*packages.Package{plain, other}) || !slices.Equal(groups[1].pkgs, []*packages.Package{local}) {
		t.Fatalf("incorrect per-initial groups: %+v", groups)
	}
	ctx.initial = []*packages.Package{plain, other}
	if groups := groupInitialBuilds(ctx, nil); len(groups) != 1 || len(groups[0].pkgs) != 2 {
		t.Fatalf("equivalent programs lost shared compilation: %+v", groups)
	}
	ctx.mode = ModeGen
	if groups := groupInitialBuilds(ctx, nil); groups != nil {
		t.Fatalf("ModeGen unexpectedly grouped: %+v", groups)
	}
	ctx.mode, ctx.initial = ModeBuild, []*packages.Package{plain}
	if groups := groupInitialBuilds(ctx, nil); groups != nil {
		t.Fatalf("single initial unexpectedly grouped: %+v", groups)
	}
}

func TestGroupInitialWasmFeatures(t *testing.T) {
	for _, provider := range []string{"wasi", "gojs", "emscripten"} {
		t.Run(provider, func(t *testing.T) {
			pkg := buildWasmReflectTestProgram(t, `package main; import ("reflect"; "runtime"); func main() { reflect.ValueOf(func() {}).Call(nil); _ = runtime.FuncForPC(0) }`)
			prog := llssa.NewProgram(&llssa.Target{GOARCH: "wasm", WasmProvider: provider})
			defer prog.Dispose()
			// The imported runtime package has no main entry and does not call
			// either API from its init. The executable does call both APIs.
			ctx := &context{prog: prog, progSSA: pkg.Prog, buildConf: &Config{BuildMode: BuildModeExe}, initial: []*packages.Package{
				{Types: pkg.Prog.ImportedPackage("runtime").Pkg}, {Types: pkg.Pkg},
			}}
			groups := groupInitialBuilds(ctx, nil)
			if len(groups) != 2 || groups[0].features.funcInfoEntries || groups[0].features.reflectBridges ||
				!groups[1].features.funcInfoEntries || groups[1].features.reflectBridges != (provider == "wasi") {
				t.Fatalf("Wasm features leaked across initial programs: %+v", groups)
			}
		})
	}
}

func TestMultiBuildRuntimeIsolationAndCache(t *testing.T) {
	t.Setenv(llgoBuildCache, "1")
	cacheDir := t.TempDir()
	oldCacheRoot := cacheRootFunc
	cacheRootFunc = func() string { return cacheDir }
	defer func() { cacheRootFunc = oldCacheRoot }()
	names := []string{"cprintf", "println", "fmtprintf"}
	baselines := make(map[string][]byte)
	fingerprints := make(map[string]string)
	build := func(names []string, warm bool) {
		t.Helper()
		conf := NewDefaultConf(ModeBuild)
		conf.OutFile = t.TempDir() + string(os.PathSeparator)
		args := make([]string, len(names))
		for i, name := range names {
			args[i] = "../../benchmark/binary_size/" + name
		}
		pkgs, err := Do(args, conf)
		if err != nil {
			t.Fatal(err)
		}
		hits := 0
		for _, pkg := range pkgs {
			if pkg.CacheHit {
				hits++
			}
			for _, name := range names {
				if pkg.PkgPath != "github.com/xgo-dev/llgo/benchmark/binary_size/"+name {
					continue
				}
				if before, ok := fingerprints[name]; ok && before != pkg.Fingerprint {
					t.Errorf("%s fingerprint changed with unrelated initials", name)
				}
				fingerprints[name] = pkg.Fingerprint
				if name == "cprintf" && pkg.NeedRt {
					t.Error("cprintf unexpectedly requires runtime")
				}
			}
		}
		if warm && hits == 0 {
			t.Fatal("warm build did not reuse package archives")
		}
		for _, name := range names {
			output := filepath.Join(conf.OutFile, name+conf.AppExt)
			raw, err := os.ReadFile(output)
			if err != nil {
				t.Fatal(err)
			}
			if before, ok := baselines[name]; ok {
				// Windows PE headers may contain link timestamps. Still require
				// the same size, package fingerprint, and working program there.
				if len(before) != len(raw) || (runtime.GOOS != "windows" && !bytes.Equal(before, raw)) {
					t.Errorf("%s artifact changed with unrelated initials/cache: %d -> %d bytes", name, len(before), len(raw))
				}
			} else {
				baselines[name] = raw
			}
			assertBuiltProgram(t, output, "Hello, world")
		}
	}
	for _, name := range names {
		build([]string{name}, false)
	}
	build(names, true)
	slices.Reverse(names)
	build(names, true)
	build([]string{"cprintf"}, true)
	// Also start with a cold grouped build, then consume those archives from
	// standalone builds: cache population order must not affect the result.
	cacheDir = t.TempDir()
	build(names, false)
	for _, name := range names {
		build([]string{name}, true)
	}
}

func TestMixedInitialGroupsOutputAndErrors(t *testing.T) {
	root := writeMultiBuildModule(t, map[string]string{
		"cmd/plain/main.go": `package main; func main() { println("plain") }`,
		"cmd/local/main.go": `package main; import "fmt"; func main() { fmt.Println("local") }`,
		"cmd/broken/main.go": `package main
import ("fmt"; _ "unsafe")
//go:linkname missing C.llgo_initial_group_missing_symbol
func missing()
func main() { fmt.Println("broken"); missing() }
`,
	})
	conf := multiBuildConfig()
	conf.BuildTrace = filepath.Join(t.TempDir(), "groups.json")
	if _, err := Build(Invocation{Args: []string{"./cmd/plain", "./cmd/local"}, Config: conf, Dir: root}); err != nil {
		t.Fatal(err)
	}
	raw, err := os.ReadFile(conf.BuildTrace)
	if err != nil {
		t.Fatal(err)
	}
	var events []buildTraceEvent
	if err := json.Unmarshal(raw, &events); err != nil {
		t.Fatal(err)
	}
	builds := 0
	for _, event := range events {
		if event.Name == "build" && event.Phase == "X" {
			builds++
		}
	}
	if builds != 3 {
		t.Fatalf("expected parent and two group builds in shared trace, got %d", builds)
	}
	conf.BuildTrace = ""
	for _, name := range []string{"plain", "local"} {
		if _, err := os.Stat(filepath.Join(root, name+conf.AppExt)); !os.IsNotExist(err) {
			t.Fatalf("split check-only build published %s: %v", name, err)
		}
	}
	conf.OutFile = t.TempDir() + string(os.PathSeparator)
	_, err = Build(Invocation{Args: []string{"./cmd/broken", "./cmd/plain", "./cmd/local"}, Config: conf, Dir: root})
	if err == nil || !strings.Contains(err.Error(), "broken") {
		t.Fatalf("group link failure was lost: %v", err)
	}
	for _, name := range []string{"plain", "local"} {
		assertBuiltProgram(t, filepath.Join(conf.OutFile, name+conf.AppExt), name)
	}
}

func TestInitialGroupUsesOriginalTestPackagePath(t *testing.T) {
	root := writeMultiBuildModule(t, map[string]string{
		"first/first_test.go": `package first; import "testing"; func TestFirst(t *testing.T) {}`,
	})
	conf := NewDefaultConf(ModeTest)
	ctx := &context{mode: ModeTest, buildConf: conf, commands: commandEnv{dir: root}}
	groups := []initialBuildGroup{{pkgs: []*packages.Package{{PkgPath: "example.com/multibuild/first.test"}}}}
	if _, err := buildInitialGroups(Invocation{}, ctx, groups); err != nil {
		t.Fatal(err)
	}
}
