package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestStaticByteArrayGlobalRoundTrip(t *testing.T) {
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_ROOT", root)

	const n = 4096
	dir := t.TempDir()
	var src strings.Builder
	src.WriteString("package main\n\nvar Table = [...]byte{")
	var first, last byte
	for i := 0; i < n; i++ {
		if i > 0 {
			src.WriteByte(',')
		}
		b := byte(i*17 + 3)
		if i == 0 {
			first = b
		}
		if i == n-1 {
			last = b
		}
		fmt.Fprintf(&src, "%d", b)
	}
	src.WriteString("}\nfunc main() {\n")
	fmt.Fprintf(&src, "\tif Table[0] != %d || Table[%d] != %d {\n\t\tpanic(\"static byte array payload was not restored\")\n\t}\n", first, n-1, last)
	src.WriteString("\tprintln(\"ok\")\n}\n")
	if err := os.WriteFile(filepath.Join(dir, "main.go"), []byte(src.String()), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module example.local/staticbytes\n\ngo 1.22\n"), 0644); err != nil {
		t.Fatal(err)
	}

	out := filepath.Join(t.TempDir(), "app")
	if runtime.GOOS == "windows" {
		out += ".exe"
	}
	conf := NewDefaultConf(ModeBuild)
	conf.OutFile = out
	if _, err := Build(Invocation{Args: []string{"."}, Dir: dir, Config: conf}); err != nil {
		t.Fatal(err)
	}
	got, err := exec.Command(out).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v\n%s", err, got)
	}
	if strings.TrimSpace(string(got)) != "ok" {
		t.Fatalf("output = %q, want ok", got)
	}
}

func TestStaticByteArrayDistinctMains(t *testing.T) {
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_ROOT", root)

	mod := t.TempDir()
	if err := os.WriteFile(filepath.Join(mod, "go.mod"), []byte("module example.local/tables\n\ngo 1.22\n"), 0644); err != nil {
		t.Fatal(err)
	}
	write := func(pkg string, fill byte) {
		t.Helper()
		dir := filepath.Join(mod, pkg)
		if err := os.MkdirAll(dir, 0755); err != nil {
			t.Fatal(err)
		}
		var src strings.Builder
		src.WriteString("package main\n\nvar Table = [...]byte{")
		for i := 0; i < 4096; i++ {
			if i > 0 {
				src.WriteByte(',')
			}
			fmt.Fprintf(&src, "%d", fill)
		}
		src.WriteString("}\nfunc main() { println(Table[0], Table[4095]) }\n")
		if err := os.WriteFile(filepath.Join(dir, "main.go"), []byte(src.String()), 0644); err != nil {
			t.Fatal(err)
		}
	}
	write("a", 1)
	write("b", 2)

	outDir := filepath.Join(t.TempDir(), "bin") + string(os.PathSeparator)
	conf := NewDefaultConf(ModeBuild)
	conf.OutFile = outDir
	if _, err := Build(Invocation{Args: []string{"./a", "./b"}, Dir: mod, Config: conf}); err != nil {
		t.Fatal(err)
	}
	for _, tc := range []struct {
		name string
		want string
	}{
		{"a", "1 1"},
		{"b", "2 2"},
	} {
		bin := filepath.Join(outDir, tc.name)
		if runtime.GOOS == "windows" {
			bin += ".exe"
		}
		got, err := exec.Command(bin).CombinedOutput()
		if err != nil {
			t.Fatalf("%s: %v\n%s", tc.name, err, got)
		}
		if strings.TrimSpace(string(got)) != tc.want {
			t.Fatalf("%s output = %q, want %q", tc.name, got, tc.want)
		}
	}
}

func TestStaticByteArrayLinknameAlias(t *testing.T) {
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_ROOT", root)

	mod := t.TempDir()
	if err := os.WriteFile(filepath.Join(mod, "go.mod"), []byte("module example.com/linkreview\n\ngo 1.22\n"), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(mod, "target"), 0755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(mod, "target", "table.go"), []byte("package target\n\nvar Table [4096]byte\n"), 0644); err != nil {
		t.Fatal(err)
	}
	var src strings.Builder
	src.WriteString("package main\n\nimport (\n\t\"example.com/linkreview/target\"\n\t_ \"unsafe\"\n)\n\n//go:linkname alias example.com/linkreview/target.Table\nvar alias = [4096]byte{")
	for i := 0; i < 4096; i++ {
		if i > 0 {
			src.WriteByte(',')
		}
		src.WriteByte('1')
	}
	src.WriteString("}\nfunc main() { println(target.Table[0], alias[0]) }\n")
	if err := os.WriteFile(filepath.Join(mod, "main.go"), []byte(src.String()), 0644); err != nil {
		t.Fatal(err)
	}

	out := filepath.Join(t.TempDir(), "app")
	if runtime.GOOS == "windows" {
		out += ".exe"
	}
	conf := NewDefaultConf(ModeBuild)
	conf.OutFile = out
	if _, err := Build(Invocation{Args: []string{"."}, Dir: mod, Config: conf}); err != nil {
		t.Fatal(err)
	}
	got, err := exec.Command(out).CombinedOutput()
	if err != nil {
		t.Fatalf("run: %v\n%s", err, got)
	}
	if strings.TrimSpace(string(got)) != "1 1" {
		t.Fatalf("output = %q, want %q", got, "1 1")
	}
}
