//go:build !llgo

package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestModeBuildMultiplePackagesMatchesGoOutputContract(t *testing.T) {
	root := writeMultiBuildModule(t, map[string]string{
		"cmd/first/main.go":  mainSource("first"),
		"cmd/second/main.go": mainSource("second"),
		"cmd/a/same/main.go": mainSource("same-a"),
		"cmd/b/same/main.go": mainSource("same-b"),
		"cmd/badlink/main.go": `package main
import _ "unsafe"
//go:linkname missing C.llgo_multi_build_missing_symbol
func missing()
func main() { missing() }
`,
		"lib/lib.go": "package lib\nfunc Value() int { return 1 }\n",
	})

	conf := multiBuildConfig()
	if _, err := Build(Invocation{Args: []string{"./..."}, Config: conf, Dir: root}); err == nil ||
		!strings.Contains(err.Error(), "example.com/multibuild/cmd/badlink") {
		t.Fatalf("multi-package build link error = %v", err)
	}
	for _, name := range []string{"first", "second", "same", "badlink"} {
		if _, err := os.Stat(filepath.Join(root, name+conf.AppExt)); !os.IsNotExist(err) {
			t.Fatalf("check-only build left output %q: %v", name, err)
		}
	}

	out := filepath.Join(root, "out")
	if err := os.Mkdir(out, 0o755); err != nil {
		t.Fatal(err)
	}
	withOutput := multiBuildConfig()
	withOutput.OutFile = out
	_, err := Build(Invocation{Args: []string{"./..."}, Config: withOutput, Dir: root})
	if err == nil {
		t.Fatal("directory build unexpectedly linked the missing symbol")
	}
	for name, want := range map[string]string{"first": "first", "second": "second"} {
		assertBuiltProgram(t, filepath.Join(out, name+withOutput.AppExt), want)
	}
	if got := runBuiltProgram(t, filepath.Join(out, "same"+withOutput.AppExt)); got != "same-a" && got != "same-b" {
		t.Fatalf("same-basename output = %q, want either main package", got)
	}
	if _, err := os.Stat(filepath.Join(out, "lib"+withOutput.AppExt)); !os.IsNotExist(err) {
		t.Fatalf("non-main package produced output: %v", err)
	}

	fileOutput := multiBuildConfig()
	fileOutput.OutFile = filepath.Join(root, "not-a-directory")
	if _, err := Build(Invocation{Args: []string{"./cmd/first", "./cmd/second"}, Config: fileOutput, Dir: root}); err == nil ||
		!strings.Contains(err.Error(), "cannot write multiple packages to non-directory") {
		t.Fatalf("multi-package -o file error = %v", err)
	}

	trailingDir := filepath.Join(root, "created") + string(os.PathSeparator)
	trailing := multiBuildConfig()
	trailing.OutFile = trailingDir
	if _, err := Build(Invocation{Args: []string{"./cmd/first", "./cmd/second"}, Config: trailing, Dir: root}); err != nil {
		t.Fatalf("trailing-directory build: %v", err)
	}
	assertBuiltProgram(t, filepath.Join(trailingDir, "first"+trailing.AppExt), "first")
	assertBuiltProgram(t, filepath.Join(trailingDir, "second"+trailing.AppExt), "second")
}

func TestModeBuildMultiplePackagesRecoversIndependentRoots(t *testing.T) {
	root := writeMultiBuildModule(t, map[string]string{
		"cmd/good/main.go": mainSource("good"),
		"cmd/bad/main.go":  "package main\nfunc main() { _ = undefinedName }\n",
		"cmd/badlink/main.go": `package main
import _ "unsafe"
//go:linkname missing C.llgo_multi_build_fallback_missing_symbol
func missing()
func main() { missing() }
`,
	})
	out := filepath.Join(root, "out")
	conf := multiBuildConfig()
	conf.OutFile = out + string(os.PathSeparator)
	_, err := Build(Invocation{Args: []string{"./..."}, Config: conf, Dir: root})
	if err == nil || !strings.Contains(err.Error(), "example.com/multibuild/cmd/bad:") ||
		!strings.Contains(err.Error(), "example.com/multibuild/cmd/badlink:") {
		t.Fatalf("multi-package compile error = %v", err)
	}
	assertBuiltProgram(t, filepath.Join(out, "good"+conf.AppExt), "good")
}

func TestModeBuildOutputDirectoryRequiresMain(t *testing.T) {
	root := writeMultiBuildModule(t, map[string]string{
		"one/one.go": "package one\n",
		"two/two.go": "package two\n",
	})
	for _, args := range [][]string{{"./..."}, {"./one"}} {
		conf := multiBuildConfig()
		conf.OutFile = filepath.Join(root, "out") + string(os.PathSeparator)
		if _, err := Build(Invocation{Args: args, Config: conf, Dir: root}); err == nil ||
			!strings.Contains(err.Error(), "no main packages to build") {
			t.Fatalf("Build(%v) non-main directory output error = %v", args, err)
		}
	}
}

func TestModeBuildSinglePackageAcceptsOutputDirectory(t *testing.T) {
	root := writeMultiBuildModule(t, map[string]string{"cmd/only/main.go": mainSource("only")})
	out := filepath.Join(root, "out")
	if err := os.Mkdir(out, 0o755); err != nil {
		t.Fatal(err)
	}
	conf := multiBuildConfig()
	conf.OutFile = out
	if _, err := Build(Invocation{Args: []string{"./cmd/only"}, Config: conf, Dir: root}); err != nil {
		t.Fatal(err)
	}
	assertBuiltProgram(t, filepath.Join(out, "only"+conf.AppExt), "only")
}

func multiBuildConfig() *Config {
	conf := NewDefaultConf(ModeBuild)
	conf.BuildParallelism = 2
	conf.PCLNMode = PCLNNone
	conf.PCLNModeSet = true
	return conf
}

func writeMultiBuildModule(t *testing.T, files map[string]string) string {
	t.Helper()
	root := t.TempDir()
	files["go.mod"] = "module example.com/multibuild\n\ngo 1.25\n"
	for name, content := range files {
		path := filepath.Join(root, filepath.FromSlash(name))
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return root
}

func mainSource(output string) string {
	return "package main\nimport \"fmt\"\nfunc main() { fmt.Println(\"" + output + "\") }\n"
}

func assertBuiltProgram(t *testing.T, path, want string) {
	t.Helper()
	if got := runBuiltProgram(t, path); got != want {
		t.Fatalf("%s output = %q, want %q", path, got, want)
	}
}

func runBuiltProgram(t *testing.T, path string) string {
	t.Helper()
	if runtime.GOOS == "windows" && filepath.Ext(path) == "" {
		path += ".exe"
	}
	out, err := exec.Command(path).CombinedOutput()
	if err != nil {
		t.Fatalf("run %s: %v\n%s", path, err, out)
	}
	return strings.TrimSpace(string(out))
}
