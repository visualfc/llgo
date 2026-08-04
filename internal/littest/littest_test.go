package littest

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestLoadSpec(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "in.go")
	writeTestFile(t, path, `// LITTEST
// CHECK: ret void
package main

func main() {}
`)
	spec, err := LoadSpec(dir)
	if err != nil {
		t.Fatal(err)
	}
	if spec.Path != path {
		t.Fatalf("spec.Path = %q, want %q", spec.Path, path)
	}
}

func TestLoadSpecReportsMissingDirectory(t *testing.T) {
	_, err := LoadSpec(filepath.Join(t.TempDir(), "missing"))
	if err == nil || errors.Is(err, ErrSpecNotFound) {
		t.Fatalf("LoadSpec missing directory error = %v", err)
	}
}

func TestLoadSpecReportsMissingMarker(t *testing.T) {
	dir := t.TempDir()
	writeTestFile(t, filepath.Join(dir, "in.go"), "package main\n")
	_, err := LoadSpec(dir)
	if !errors.Is(err, ErrSpecNotFound) {
		t.Fatalf("LoadSpec error = %v, want ErrSpecNotFound", err)
	}
}

func TestLoadSpecRejectsMultipleMarkedSources(t *testing.T) {
	dir := t.TempDir()
	writeTestFile(t, filepath.Join(dir, "a.go"), "// LITTEST\npackage main\n")
	writeTestFile(t, filepath.Join(dir, "b.go"), "// LITTEST\npackage main\n")
	if _, err := LoadSpec(dir); err == nil {
		t.Fatal("LoadSpec succeeded unexpectedly")
	}
}

func TestLoadSpecMarkerRules(t *testing.T) {
	tests := []struct {
		name string
		file string
		text string
	}{
		{name: "not first line", file: "in.go", text: "\n// LITTEST\npackage main\n"},
		{name: "not Go comment", file: "in.go", text: "# LITTEST\npackage main\n"},
		{name: "not Go source", file: "in.c", text: "// LITTEST\n"},
		{name: "test file", file: "in_test.go", text: "// LITTEST\npackage main\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			dir := t.TempDir()
			writeTestFile(t, filepath.Join(dir, tt.file), tt.text)
			_, err := LoadSpec(dir)
			if !errors.Is(err, ErrSpecNotFound) {
				t.Fatalf("LoadSpec error = %v, want ErrSpecNotFound", err)
			}
		})
	}
}

func TestCheck(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "in.go")
	writeTestFile(t, path, "// CHECK: ret void\n")
	spec := Spec{Path: path}
	if err := Check(spec, "  ret void\n"); err != nil {
		t.Fatal(err)
	}
	if err := Check(spec, "  unreachable\n"); err == nil {
		t.Fatal("Check succeeded unexpectedly")
	}
}

func TestCheckReportsMalformedDirective(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "in.go")
	writeTestFile(t, path, "// CHECK: {{[invalid\n")
	err := Check(Spec{Path: path}, "")
	if err == nil || !strings.Contains(err.Error(), "found start of regex string with no end") {
		t.Fatalf("Check error = %v", err)
	}
}

func TestHasMarker(t *testing.T) {
	dir := t.TempDir()

	ok, err := HasMarker(filepath.Join(dir, "missing.go"))
	if err == nil || ok {
		t.Fatalf("HasMarker(missing) = (%v, %v)", ok, err)
	}

	tests := []struct {
		name string
		text string
		want bool
	}{
		{name: "empty"},
		{name: "plain", text: "package main\n"},
		{name: "marker", text: "// LITTEST\npackage main\n", want: true},
		{name: "trimmed marker", text: " //  LITTEST  \npackage main\n", want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			path := filepath.Join(dir, tt.name+".go")
			writeTestFile(t, path, tt.text)
			got, err := HasMarker(path)
			if err != nil || got != tt.want {
				t.Fatalf("HasMarker = (%v, %v), want (%v, nil)", got, err, tt.want)
			}
		})
	}
}

func writeTestFile(t *testing.T, path, text string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(text), 0o644); err != nil {
		t.Fatal(err)
	}
}
