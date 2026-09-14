//go:build !llgo

package gotool

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

func TestFind(t *testing.T) {
	root := t.TempDir()
	name := executableName(runtime.GOOS)
	write := func(dir string) string {
		t.Helper()
		if err := os.MkdirAll(dir, 0755); err != nil {
			t.Fatal(err)
		}
		path := filepath.Join(dir, name)
		if err := os.WriteFile(path, []byte("executable"), 0755); err != nil {
			t.Fatal(err)
		}
		return path
	}
	self := write(filepath.Join(root, "self"))
	tool := write(filepath.Join(root, "tool"))
	pathEnv := strings.Join([]string{".", filepath.Join(root, "missing"), filepath.Dir(self), filepath.Dir(tool)}, string(os.PathListSeparator))
	if got, err := Find(self, pathEnv); err != nil || got != tool {
		t.Fatalf("Find = %q, %v; want %q", got, err, tool)
	}
	if _, err := Find(self, filepath.Dir(self)); err == nil {
		t.Fatal("self-only PATH should fail")
	}
	if _, err := Find(filepath.Join(root, "missing-self"), pathEnv); err == nil {
		t.Fatal("missing self should fail")
	}
	dir := t.TempDir()
	if err := os.Symlink(self, filepath.Join(dir, name)); err != nil {
		t.Skipf("cannot create symlink: %v", err)
	}
	if _, err := Find(self, dir); err == nil {
		t.Fatal("symlink to self should fail")
	}
	t.Setenv(childGuard, "1")
	if _, err := Find(self, filepath.Dir(tool)); err == nil || !strings.Contains(err.Error(), "recursive") {
		t.Fatalf("guarded Find error = %v", err)
	}
}

func TestExecutableName(t *testing.T) {
	if got := executableName("windows"); got != "go.exe" {
		t.Errorf("Windows name = %q", got)
	}
	if got := executableName("linux"); got != "go" {
		t.Errorf("Unix name = %q", got)
	}
}

func TestChildEnv(t *testing.T) {
	got := ChildEnv([]string{"PATH=/bin"})
	if len(got) != 2 || got[0] != "PATH=/bin" || got[1] != childGuard+"=1" {
		t.Fatalf("ChildEnv = %q", got)
	}
}
