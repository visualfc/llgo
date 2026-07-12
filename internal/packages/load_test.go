//go:build !llgo

package packages

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestLoadExWithGoVersion(t *testing.T) {
	dir := t.TempDir()
	writeLoadTestFile(t, filepath.Join(dir, "go.mod"), "module example.com/loadversion\ngo 1.24\n")
	writeLoadTestFile(t, filepath.Join(dir, "load.go"), `package loadversion

func identity[T any](value T) T { return value }
`)

	oldVersion, err := LoadExWithGoVersion(nil, nil, loadTestConfig(dir), "go1.17", ".")
	if err != nil {
		t.Fatal(err)
	}
	if len(oldVersion) != 1 {
		t.Fatalf("LoadExWithGoVersion returned %d packages, want 1", len(oldVersion))
	}
	if !oldVersion[0].IllTyped || !packageErrorsContain(oldVersion[0], "requires go1.18 or later") {
		t.Fatalf("go1.17 load did not reject generics: %+v", oldVersion[0].Errors)
	}

	currentVersion, err := LoadEx(nil, nil, loadTestConfig(dir), ".")
	if err != nil {
		t.Fatal(err)
	}
	if len(currentVersion) != 1 {
		t.Fatalf("LoadEx returned %d packages, want 1", len(currentVersion))
	}
	pkg := currentVersion[0]
	if pkg.IllTyped || len(pkg.Errors) != 0 {
		t.Fatalf("module-version load failed: %+v", pkg.Errors)
	}
	if len(pkg.Syntax) != 1 || pkg.TypesInfo == nil {
		t.Fatalf("load did not return syntax and type information: %+v", pkg)
	}
	if got := pkg.TypesInfo.FileVersions[pkg.Syntax[0]]; got != "go1.24" {
		t.Fatalf("file language version = %q, want go1.24", got)
	}
}

func loadTestConfig(dir string) *Config {
	return &Config{
		Dir: dir,
		Mode: NeedName | NeedFiles | NeedCompiledGoFiles | NeedSyntax |
			NeedTypes | NeedTypesInfo | NeedTypesSizes | NeedModule,
	}
}

func packageErrorsContain(pkg *Package, text string) bool {
	for _, err := range pkg.Errors {
		if strings.Contains(err.Msg, text) {
			return true
		}
	}
	return false
}

func writeLoadTestFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
}
