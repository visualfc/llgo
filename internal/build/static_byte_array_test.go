package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
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
