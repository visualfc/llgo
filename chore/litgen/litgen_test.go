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

func TestProcessPath_UsesFlagsFileTarget(t *testing.T) {
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	pkgDir, err := os.MkdirTemp(wd, "flags-target-")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		_ = os.RemoveAll(pkgDir)
	})
	sourceFile := filepath.Join(pkgDir, "in.go")
	if err := os.WriteFile(sourceFile, []byte("// LITTEST\npackage main\n\nfunc main() { defer func() {}() }\n"), 0644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(pkgDir, "flags.txt"), []byte("-target=wasm\n"), 0644); err != nil {
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
	if !strings.Contains(text, "// CHECK-NEXT:   switch i32") {
		t.Fatalf("litgen did not use the wasm target from flags.txt:\n%s", text)
	}
	if strings.Contains(text, "blockaddress") || strings.Contains(text, "indirectbr") {
		t.Fatalf("litgen generated native defer dispatch for a wasm target:\n%s", text)
	}
}
