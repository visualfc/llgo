package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestProcessPath_SingleFileUsesContainingDir(t *testing.T) {
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	pkgDir, err := os.MkdirTemp(wd, "processpath-")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		_ = os.RemoveAll(pkgDir)
	})
	sourceFile := filepath.Join(pkgDir, "in.go")
	if err := os.WriteFile(sourceFile, []byte("// LITTEST\npackage main\n\nfunc main() { helper() }\n"), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkgDir, "helper.go"), []byte("package main\n\nfunc helper() {}\n"), 0644); err != nil {
		t.Fatal(err)
	}

	if err := processPath(sourceFile); err != nil {
		t.Fatal(err)
	}

	data, err := os.ReadFile(sourceFile)
	if err != nil {
		t.Fatal(err)
	}
	text := string(data)
	if strings.Contains(text, "command-line-arguments") {
		t.Fatalf("single-file mode should compile the containing package, got:\n%s", text)
	}
	want := `// CHECK-LABEL: define void @main.main(){{.*}} {`
	if !strings.Contains(text, want) {
		t.Fatalf("missing canonical main check:\n%s", text)
	}
	if !strings.Contains(text, `call void @main.helper()`) {
		t.Fatalf("single-file mode did not include the containing package:\n%s", text)
	}
}
