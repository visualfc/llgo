//go:build !llgo

package build

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"go/parser"
	"go/token"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
	"sync"
	"testing"

	"github.com/xgo-dev/llgo/internal/lto"
	"github.com/xgo-dev/llgo/internal/optlevel"
	"golang.org/x/tools/cover"
)

func TestCoverageRegistration(t *testing.T) {
	const source = `package p

var GoCoverP uint32
var GoCover_0 [4]uint32
var GoCover_1 [5]uint32
var GoCoverM = [4]byte{1, 2, 3, 4}
`
	fix := coverFixupConfig{
		MetaVar:            "GoCoverM",
		MetaLen:            4,
		MetaHash:           "00112233445566778899aabbccddeeff",
		Strategy:           "normal",
		CounterPrefix:      "GoCover",
		PkgIdVar:           "GoCoverP",
		CounterMode:        "atomic",
		CounterGranularity: "perblock",
	}
	data, err := coverageRegistration([]byte(source), fix, "example.org/p", -1)
	if err != nil {
		t.Fatal(err)
	}
	for _, required := range []string{
		"var GoCoverCounters = [...]struct",
		"{&GoCover_0[0], uint64(len(GoCover_0))}",
		"{&GoCover_1[0], uint64(len(GoCover_1))}",
		"var GoCoverP = GoCoverregister(",
		"runtime.registerCoverage",
	} {
		if !bytes.Contains(data, []byte(required)) {
			t.Errorf("missing %q:\n%s", required, data)
		}
	}
	for name, change := range map[string]func(*coverFixupConfig){
		"strategy":    func(f *coverFixupConfig) { f.Strategy = "future" },
		"granularity": func(f *coverFixupConfig) { f.CounterGranularity = "perfunc" },
		"hash":        func(f *coverFixupConfig) { f.MetaHash = "not hex" },
		"hash length": func(f *coverFixupConfig) { f.MetaHash = "00" },
		"mode":        func(f *coverFixupConfig) { f.CounterMode = "future" },
		"pkg ID":      func(f *coverFixupConfig) { f.PkgIdVar = "missing" },
	} {
		t.Run(name, func(t *testing.T) {
			invalid := fix
			change(&invalid)
			if _, err := coverageRegistration([]byte(source), invalid, "p", -1); err == nil {
				t.Fatal("accepted unsupported fixup protocol")
			}
		})
	}
	if _, err := coverageRegistration([]byte("bad Go"), fix, "p", -1); err == nil {
		t.Fatal("accepted malformed generated source")
	}
}

func TestCoveragePackageIDs(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "src/internal/coverage/pkid.go")
	if err := os.MkdirAll(filepath.Dir(path), 0700); err != nil {
		t.Fatal(err)
	}
	for _, source := range []string{
		"package coverage\nvar rtPkgs = [...]string{\"runtime\", \"internal/abi\"}\n",
		"package coverage\nvar rtPkgs = [...]string{someConstant}\n",
		"package coverage\n",
	} {
		if err := os.WriteFile(path, []byte(source), 0600); err != nil {
			t.Fatal(err)
		}
		ids, err := coveragePackageIDs(root)
		if strings.Contains(source, `"runtime"`) {
			if err != nil || ids["runtime"] != -2 || ids["internal/abi"] != -3 {
				t.Fatalf("IDs = %v, %v", ids, err)
			}
		} else if err == nil {
			t.Fatalf("accepted incompatible table: %s", source)
		}
	}
}

func TestCoverageMainTemplates(t *testing.T) {
	for name, source := range map[string]string{
		"build":        coverageBuildMain,
		"build legacy": coverageBuildMainLegacy + coverageLegacyExitSupport,
		"current":      fmt.Sprintf(coverageTestMain, "set", "", []string{"p"}),
		"go120":        legacyCoverageMain("count", " in ./...", true),
		"go121":        legacyCoverageMain("atomic", "", false),
	} {
		t.Run(name, func(t *testing.T) {
			if _, err := parser.ParseFile(token.NewFileSet(), "main.go", source+coverageExitSupport, parser.AllErrors); err != nil {
				t.Fatal(err)
			}
			if name == "go120" && strings.Contains(source, "llgoCoverTearDown, llgoCoverSnapshot") {
				t.Fatal("Go 1.20 uses the newer snapshot callback ABI")
			}
		})
	}
	body, err := addCoverageImport([]byte("package p\nfunc F() {}\n"), "_", "example.org/counters")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := parser.ParseFile(token.NewFileSet(), "p.go", body, parser.AllErrors); err != nil {
		t.Fatal(err)
	}
	if _, err := addCoverageImport([]byte("not Go"), "_", "p"); err == nil {
		t.Fatal("accepted invalid instrumented source")
	}
	source := []byte("//line /original/p.go:20\npackage p\n\nfunc F() {}\n")
	body, err = addCoverageImport(source, "_", "example.org/counters")
	if err != nil {
		t.Fatal(err)
	}
	var positions []token.Position
	for _, input := range [][]byte{source, body} {
		fset := token.NewFileSet()
		file, err := parser.ParseFile(fset, "generated.go", input, 0)
		if err != nil {
			t.Fatal(err)
		}
		decl := file.Decls[len(file.Decls)-1]
		positions = append(positions, fset.Position(decl.Pos()))
	}
	if positions[0].String() != positions[1].String() {
		t.Fatalf("added import shifted source location: %v", positions)
	}
	atomic := []byte("//line /original/p.go:20\npackage p; import _cover_atomic_ \"sync/atomic\"\nfunc F() {}\n")
	body, err = addCoverageImport(atomic, "_", "example.org/counters")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := parser.ParseFile(token.NewFileSet(), "p.go", body, parser.AllErrors); err != nil {
		t.Fatalf("atomic coverage import: %v\n%s", err, body)
	}
}

func TestCoverageOptions(t *testing.T) {
	dir := t.TempDir()
	commands := commandEnv{dir: dir}
	if got, err := newCoverageBuild(&Config{}, commands); got != nil || err != nil {
		t.Fatalf("disabled coverage = %v, %v", got, err)
	}
	for _, mode := range []string{"", "set", "count", "atomic"} {
		t.Run("mode="+mode, func(t *testing.T) {
			conf := &Config{
				Mode: ModeTest,
				Coverage: &CoverageConfig{
					Mode:    mode,
					Profile: "cover.out",
				},
			}
			c, err := newCoverageBuild(conf, commands)
			if err != nil {
				t.Fatal(err)
			}
			defer c.close()
			if mode == "" {
				mode = "set"
			}
			if c.options.Mode != mode || c.options.Profile != filepath.Join(dir, "cover.out") {
				t.Fatalf("resolved options: %+v", c.options)
			}
			data, err := os.ReadFile(c.options.Profile)
			if err != nil || string(data) != "mode: "+mode+"\n" {
				t.Fatalf("profile = %q, %v", data, err)
			}
		})
	}
	for name, conf := range map[string]*Config{
		"bad mode": {
			Mode:     ModeTest,
			Coverage: &CoverageConfig{Mode: "invalid"},
		},
		"run": {
			Mode:     ModeRun,
			Coverage: &CoverageConfig{},
		},
		"build profile": {
			Mode:     ModeBuild,
			Coverage: &CoverageConfig{Profile: "cover.out"},
		},
		"shared library": {
			Mode:      ModeBuild,
			BuildMode: BuildModeCShared,
			Coverage:  &CoverageConfig{},
		},
		"target": {
			Mode:     ModeTest,
			Target:   "wasi",
			Coverage: &CoverageConfig{},
		},
		"fuzz profile": {
			Mode:     ModeTest,
			RunArgs:  []string{"-test.fuzz=Fuzz"},
			Coverage: &CoverageConfig{Profile: "cover.out"},
		},
	} {
		t.Run(name, func(t *testing.T) {
			if c, err := newCoverageBuild(conf, commands); err == nil {
				c.close()
				t.Fatal("expected error")
			}
		})
	}
	t.Run("compile only", func(t *testing.T) {
		profile := filepath.Join(dir, "must-not-exist.out")
		c, err := newCoverageBuild(&Config{
			Mode:        ModeTest,
			CompileOnly: true,
			Coverage:    &CoverageConfig{Profile: profile},
		}, commands)
		if err != nil {
			t.Fatal(err)
		}
		defer c.close()
		if _, err := os.Stat(profile); !os.IsNotExist(err) {
			t.Fatalf("-c created a profile: %v", err)
		}
	})
}

func TestCoverageFailureResult(t *testing.T) {
	if err := coverageTestFailure([]error{nil}); err != ErrTestFailed {
		t.Fatalf("test failure = %v", err)
	}
	buildErr := errors.New("build failed")
	if err := coverageTestFailure([]error{nil, buildErr}); !errors.Is(err, buildErr) {
		t.Fatalf("lost build error: %v", err)
	}
}

func TestCoverageParallelProfiles(t *testing.T) {
	dir := t.TempDir()
	c, err := newCoverageBuild(&Config{
		Mode:     ModeTest,
		Coverage: &CoverageConfig{Profile: "cover.out", OutputDir: dir},
	}, commandEnv{dir: dir})
	if err != nil {
		t.Fatal(err)
	}
	defer c.close()
	c.manifest = []byte(`{"ImportPaths":[],"MetaFileFragments":[]}`)
	const runs = 16
	var wg sync.WaitGroup
	for i := range runs {
		wg.Add(1)
		go func() {
			defer wg.Done()
			args, profile, err := c.runArgs([]string{"-test.run=Test"})
			if err != nil {
				t.Error(err)
				return
			}
			if len(args) != 3 || args[2] != "-test.run=Test" {
				t.Errorf("args: %v", args)
			}
			manifest, err := os.ReadFile(filepath.Join(filepath.Dir(profile), "metafiles.txt"))
			if err != nil || !bytes.Equal(manifest, c.manifest) {
				t.Errorf("manifest: %s, %v", manifest, err)
			}
			data := fmt.Sprintf("mode: set\np%d.go:1.1,2.1 1 1\n", i)
			if err := os.WriteFile(profile, []byte(data), 0600); err != nil {
				t.Error(err)
				return
			}
			if err := c.merge(profile); err != nil {
				t.Error(err)
			}
		}()
	}
	wg.Wait()
	data, err := os.ReadFile(c.options.Profile)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Count(string(data), "mode: ") != 1 || bytes.Count(data, []byte("\n")) != runs+1 {
		t.Fatalf("invalid merged profile:\n%s", data)
	}
	if err := c.merge(filepath.Join(dir, "missing.out")); err != nil {
		t.Fatal(err)
	}
	bad := filepath.Join(dir, "bad.out")
	if err := os.WriteFile(bad, []byte("mode: atomic\n"), 0600); err != nil {
		t.Fatal(err)
	}
	if err := c.merge(bad); err == nil {
		t.Fatal("accepted a mismatched profile mode")
	}
	if err := os.WriteFile(bad, nil, 0600); err != nil {
		t.Fatal(err)
	}
	if err := c.merge(bad); err != nil {
		t.Fatalf("empty profile from an interrupted test: %v", err)
	}
}

func TestCoverageAgainstGo(t *testing.T) {
	if testing.Short() {
		t.Skip("builds and runs native test binaries")
	}
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_ROOT", root)
	t.Setenv(llgoBuildCache, "1")
	fingerprints := make(map[string]string)
	for _, mode := range []string{"set", "count", "atomic"} {
		t.Run(mode, func(t *testing.T) {
			dir := t.TempDir()
			fixture := filepath.Join(root, "internal/build/testdata/coverage/basic")
			goProfile := filepath.Join(dir, "go.out")
			cmd := exec.Command("go", "test", "-count=1", "-covermode="+mode, "-coverprofile="+goProfile, ".")
			cmd.Dir = fixture
			goOutput, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go test: %v\n%s", err, goOutput)
			}
			conf := NewDefaultConf(ModeTest)
			conf.CompileOnly = true
			conf.OutFile = filepath.Join(dir, "basic.test")
			if runtime.GOOS == "windows" {
				conf.OutFile += ".exe"
			}
			conf.Coverage = &CoverageConfig{Mode: mode}
			inv := Invocation{Args: []string{"."}, Dir: fixture, Config: conf}
			pkgs, err := Build(inv)
			if err != nil {
				t.Fatal(err)
			}
			for _, pkg := range pkgs {
				if pkg.Name == "basic" {
					if previous := fingerprints[pkg.Fingerprint]; previous != "" {
						t.Fatalf("%s shares a cache key with %s", mode, previous)
					}
					fingerprints[pkg.Fingerprint] = mode
				}
			}
			pkgs, err = Build(inv)
			if err != nil {
				t.Fatal(err)
			}
			var found bool
			for _, pkg := range pkgs {
				if pkg.Name == "basic" {
					found = true
					if !pkg.CacheHit {
						t.Error("identical covered package missed the archive cache")
					}
				}
			}
			if !found {
				t.Fatal("covered package not built")
			}
			llgoProfile := filepath.Join(dir, "llgo.out")
			cmd = exec.Command(conf.OutFile, "-test.coverprofile="+llgoProfile)
			llgoOutput, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("llgo test binary: %v\n%s", err, llgoOutput)
			}
			for _, output := range [][]byte{goOutput, llgoOutput} {
				if !bytes.Contains(output, []byte("coverage: 50.0% of statements")) {
					t.Fatalf("unexpected coverage: %s", output)
				}
			}
			compareCoverageProfiles(t, goProfile, llgoProfile)
		})
	}
	conf := NewDefaultConf(ModeTest)
	conf.OutFile = filepath.Join(t.TempDir(), "plain.test")
	pkgs, err := Build(Invocation{
		Args:   []string{"./internal/build/testdata/coverage/basic"},
		Dir:    root,
		Config: conf,
	})
	if err != nil {
		t.Fatal(err)
	}
	for _, pkg := range pkgs {
		if pkg.Name == "basic" && fingerprints[pkg.Fingerprint] != "" {
			t.Fatal("ordinary package reused an instrumented archive key")
		}
	}
}

func compareCoverageProfiles(t *testing.T, wantFile, gotFile string) {
	t.Helper()
	want, err := cover.ParseProfiles(wantFile)
	if err != nil {
		t.Fatal(err)
	}
	got, err := cover.ParseProfiles(gotFile)
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(got, want) {
		wantJSON, _ := json.MarshalIndent(want, "", "  ")
		gotJSON, _ := json.MarshalIndent(got, "", "  ")
		t.Fatalf("coverage mismatch\nwant: %s\ngot: %s", wantJSON, gotJSON)
	}
}

func TestCoverageLTO(t *testing.T) {
	if testing.Short() {
		t.Skip("builds optimized test binaries")
	}
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_ROOT", root)
	for _, mode := range []lto.Mode{lto.Thin, lto.Full} {
		t.Run(mode.String(), func(t *testing.T) {
			conf := NewDefaultConf(ModeTest)
			conf.Coverage = &CoverageConfig{}
			conf.LTO = mode
			conf.OptLevel = optlevel.O2
			conf.OutFile = filepath.Join(t.TempDir(), "basic.test")
			_, err := Build(Invocation{
				Args:   []string{"./internal/build/testdata/coverage/basic"},
				Dir:    root,
				Config: conf,
			})
			if err != nil {
				t.Fatal(err)
			}
		})
	}
}

func TestCoverageMultiPackage(t *testing.T) {
	if testing.Short() {
		t.Skip("builds and runs several test binaries")
	}
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_ROOT", root)
	t.Setenv(llgoBuildCache, "1")
	pattern := "./internal/build/testdata/coverage/..."
	for _, selection := range []string{"", pattern} {
		t.Run("coverpkg="+selection, func(t *testing.T) {
			dir := t.TempDir()
			goProfile := filepath.Join(dir, "go.out")
			args := []string{"test", "-count=1", "-coverprofile=" + goProfile}
			if selection != "" {
				args = append(args, "-coverpkg="+selection)
			}
			cmd := exec.Command("go", append(args, pattern)...)
			cmd.Dir = root
			if output, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("go test: %v\n%s", err, output)
			}
			conf := NewDefaultConf(ModeTest)
			conf.BuildParallelism = 2
			conf.Coverage = &CoverageConfig{
				Packages: selection,
				Profile:  filepath.Join(dir, "llgo.out"),
			}
			conf.TestJSON = true
			conf.BuildTrace = filepath.Join(dir, "trace.json")
			// Neither test binary can finish until the other has started.
			t.Setenv("LLGO_COVER_BARRIER", t.TempDir())
			output := captureCoverageOutput(t, func() {
				if _, err := Build(Invocation{Args: []string{pattern}, Dir: root, Config: conf}); err != nil {
					t.Error(err)
				}
			})
			compareCoverageProfiles(t, goProfile, conf.Coverage.Profile)
			trace, err := os.ReadFile(conf.BuildTrace)
			if err != nil {
				t.Fatal(err)
			}
			var events []buildTraceEvent
			if err := json.Unmarshal(trace, &events); err != nil {
				t.Fatal(err)
			}
			var instrumented bool
			for _, event := range events {
				if strings.HasPrefix(event.Name, "coverage ") && event.Phase == "X" {
					instrumented = true
				}
			}
			if !instrumented {
				t.Fatal("build trace does not include coverage instrumentation")
			}
			var passed int
			for _, line := range bytes.Split(bytes.TrimSpace(output), []byte("\n")) {
				var event struct {
					Action  string
					Test    string
					Elapsed float64
				}
				if err := json.Unmarshal(line, &event); err != nil {
					t.Fatalf("invalid test2json event: %s: %v", line, err)
				}
				if event.Action == "pass" && event.Test == "" {
					passed++
					if event.Elapsed <= 0 {
						t.Errorf("missing package elapsed time: %s", line)
					}
				}
			}
			if passed != 4 {
				t.Fatalf("got %d passed test packages:\n%s", passed, output)
			}
		})
	}
}

func captureCoverageOutput(t *testing.T, run func()) []byte {
	t.Helper()
	f, err := os.CreateTemp(t.TempDir(), "stdout")
	if err != nil {
		t.Fatal(err)
	}
	old := os.Stdout
	os.Stdout = f
	defer func() {
		os.Stdout = old
		f.Close()
	}()
	run()
	if err := f.Close(); err != nil {
		t.Fatal(err)
	}
	os.Stdout = old
	output, err := os.ReadFile(f.Name())
	if err != nil {
		t.Fatal(err)
	}
	return output
}

func TestCoverageAPIsAndExit(t *testing.T) {
	if testing.Short() {
		t.Skip("builds and runs covered test binaries")
	}
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_ROOT", root)
	for _, mode := range []string{"set", "atomic"} {
		t.Run(mode, func(t *testing.T) {
			dir := t.TempDir()
			conf := NewDefaultConf(ModeTest)
			conf.Coverage = &CoverageConfig{Mode: mode}
			conf.CompileOnly = true
			conf.OutFile = filepath.Join(dir, "api.test")
			if runtime.GOOS == "windows" {
				conf.OutFile += ".exe"
			}
			_, err := Build(Invocation{
				Args:   []string{"./internal/build/testdata/coverage/api"},
				Dir:    root,
				Config: conf,
			})
			if err != nil {
				t.Fatal(err)
			}
			cmd := exec.Command(conf.OutFile)
			cmd.Env = append(os.Environ(), "LLGO_COVER_TEST_APIS=1")
			if output, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("coverage APIs: %v\n%s", err, output)
			}
			cmd = exec.Command(conf.OutFile)
			cmd.Env = append(os.Environ(), "LLGO_COVER_EARLY_EXIT=1", "GOCOVERDIR="+dir)
			if output, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("early exit: %v\n%s", err, output)
			}
			cmd = exec.Command("go", "tool", "covdata", "percent", "-i="+dir)
			output, err := cmd.CombinedOutput()
			if err != nil || !bytes.Contains(output, []byte("coverage: 66.7% of statements")) {
				t.Fatalf("early-exit counters: %v\n%s", err, output)
			}
			profile := filepath.Join(dir, "failed.out")
			cmd = exec.Command(conf.OutFile, "-test.run=TestFailureProfile", "-test.coverprofile="+profile)
			cmd.Env = append(os.Environ(), "LLGO_COVER_TEST_FAIL=1")
			if output, err := cmd.CombinedOutput(); err == nil || !bytes.Contains(output, []byte("intentional coverage failure")) {
				t.Fatalf("failure fixture: %v\n%s", err, output)
			}
			profiles, err := cover.ParseProfiles(profile)
			if err != nil || len(profiles) == 0 {
				t.Fatalf("missing failure profile: %v", err)
			}
		})
	}
}

func TestCoverageFailedPackage(t *testing.T) {
	if testing.Short() {
		t.Skip("builds and runs failing covered tests")
	}
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_ROOT", root)
	t.Setenv("LLGO_COVER_TEST_FAIL", "1")
	for _, jsonOutput := range []bool{false, true} {
		t.Run(fmt.Sprintf("json=%t", jsonOutput), func(t *testing.T) {
			profile := filepath.Join(t.TempDir(), "failed.out")
			conf := NewDefaultConf(ModeTest)
			conf.Coverage = &CoverageConfig{Profile: profile}
			conf.TestJSON = jsonOutput
			conf.RunArgs = []string{"-test.run=TestFailureProfile"}
			output := captureCoverageOutput(t, func() {
				_, err := Build(Invocation{
					Args:   []string{"./internal/build/testdata/coverage/api"},
					Dir:    root,
					Config: conf,
				})
				if err != ErrTestFailed {
					t.Fatalf("test failure = %v", err)
				}
			})
			if !bytes.Contains(output, []byte("intentional coverage failure")) ||
				!bytes.Contains(output, []byte("coverage: 66.7% of statements")) {
				t.Fatalf("missing failure or coverage report:\n%s", output)
			}
			if bytes.Contains(output, []byte("exit code")) {
				t.Fatalf("LLGo-specific exit diagnostic in test output:\n%s", output)
			}
			if jsonOutput {
				failed := false
				for _, line := range bytes.Split(bytes.TrimSpace(output), []byte("\n")) {
					var event struct {
						Action string
						Test   string
					}
					if err := json.Unmarshal(line, &event); err != nil {
						t.Fatalf("non-JSON output: %s", line)
					}
					failed = failed || event.Action == "fail" && event.Test == ""
				}
				if !failed {
					t.Fatalf("missing package failure event:\n%s", output)
				}
			}
			profiles, err := cover.ParseProfiles(profile)
			if err != nil || len(profiles) == 0 {
				t.Fatalf("failed package profile: %v", err)
			}
		})
	}
}

func TestCoverageStandardLibrary(t *testing.T) {
	if testing.Short() {
		t.Skip("instruments the standard-library dependency graph")
	}
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_ROOT", root)
	dir := t.TempDir()
	profile := filepath.Join(dir, "cover.out")
	conf := NewDefaultConf(ModeTest)
	conf.Coverage = &CoverageConfig{
		Mode:     "atomic",
		Packages: "all",
		Profile:  profile,
	}
	_, err = Build(Invocation{
		Args:   []string{"./internal/build/testdata/coverage/basic"},
		Dir:    root,
		Config: conf,
	})
	if err != nil {
		t.Fatal(err)
	}
	for _, args := range [][]string{
		{"tool", "cover", "-func=" + profile},
		{"tool", "cover", "-html=" + profile, "-o=" + filepath.Join(dir, "cover.html")},
	} {
		cmd := exec.Command("go", args...)
		cmd.Dir = root
		if output, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("profile has invalid source coordinates: %v\n%s", err, output)
		}
	}
}
