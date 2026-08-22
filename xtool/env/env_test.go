package env

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"strings"
	"testing"
)

const helperEnvironment = "LLGO_ENV_TEST_HELPER"

func TestMain(m *testing.M) {
	if os.Getenv(helperEnvironment) == "1" {
		dir, err := os.Getwd()
		if err != nil {
			os.Exit(2)
		}
		fmt.Printf("-L%s -I%s", os.Getenv("LLGO_ENV_TEST"), dir)
		os.Exit(0)
	}
	os.Exit(m.Run())
}

func TestExpandEnvToArgsWithUsesExplicitEnvironment(t *testing.T) {
	t.Setenv("LLGO_ENV_TEST", "ambient")
	got := ExpandEnvToArgsWith("$LLGO_ENV_TEST", "", []string{"LLGO_ENV_TEST=request"})
	if want := []string{"request"}; !reflect.DeepEqual(got, want) {
		t.Fatalf("ExpandEnvToArgsWith = %q, want %q", got, want)
	}
}

func TestExpandEnvUsesProcessEnvironment(t *testing.T) {
	t.Setenv("LLGO_ENV_TEST", "ambient")
	if got := ExpandEnv("$LLGO_ENV_TEST"); got != "ambient" {
		t.Fatalf("ExpandEnv = %q, want %q", got, "ambient")
	}
	if got := ExpandEnvToArgs("$LLGO_ENV_TEST"); !reflect.DeepEqual(got, []string{"ambient"}) {
		t.Fatalf("ExpandEnvToArgs = %q, want %q", got, []string{"ambient"})
	}
	if got := ExpandEnvToArgs(""); got != nil {
		t.Fatalf("ExpandEnvToArgs(empty) = %q, want nil", got)
	}
}

func TestExpandEnvToArgsWithConfiguresSubprocess(t *testing.T) {
	dir := t.TempDir()
	tool := filepath.Join(dir, "pkg-config")
	if runtime.GOOS == "windows" {
		tool += ".exe"
	}
	copyExecutable(t, tool)
	got := ExpandEnvToArgsWith(
		"$(pkg-config --libs fixture)",
		dir,
		[]string{"PATH=" + dir, "LLGO_ENV_TEST=request", helperEnvironment + "=1"},
	)
	if len(got) != 2 || got[0] != "-Lrequest" || !strings.HasPrefix(got[1], "-I") {
		t.Fatalf("ExpandEnvToArgsWith = %q, want -Lrequest and one include directory", got)
	}
	gotInfo, gotErr := os.Stat(strings.TrimPrefix(got[1], "-I"))
	wantInfo, wantErr := os.Stat(dir)
	if gotErr != nil || wantErr != nil || !os.SameFile(gotInfo, wantInfo) {
		t.Fatalf("subprocess working directory = %q, want same directory as %q", got[1][2:], dir)
	}
}

func TestLookPathInEnvironmentBoundaries(t *testing.T) {
	dir := t.TempDir()
	toolName := "fixture-tool"
	tool := filepath.Join(dir, toolName)
	if runtime.GOOS == "windows" {
		tool += ".exe"
	}
	if err := os.WriteFile(tool, []byte("#!/bin/sh\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	got := lookPathInEnvironment(toolName, dir, []string{"PATH=" + string(os.PathListSeparator)})
	gotInfo, gotErr := os.Stat(got)
	wantInfo, wantErr := os.Stat(tool)
	if gotErr != nil || wantErr != nil || !os.SameFile(gotInfo, wantInfo) {
		t.Fatalf("lookPathInEnvironment with empty entry = %q, want same file as %q", got, tool)
	}
	if got := lookPathInEnvironment(filepath.Join("bin", "tool"), dir, nil); got != filepath.Join("bin", "tool") {
		t.Fatalf("lookPathInEnvironment with separator = %q", got)
	}
	if got := lookPathInEnvironment("missing-tool", dir, []string{"PATH=" + t.TempDir()}); got != "missing-tool" {
		t.Fatalf("lookPathInEnvironment missing tool = %q", got)
	}
	if got := ExpandEnvToArgsWith("$LLGO_ENV_MISSING", dir, []string{"PATH=" + dir}); got != nil {
		t.Fatalf("missing explicit environment variable = %q, want nil", got)
	}
}

func copyExecutable(t *testing.T, dst string) {
	t.Helper()
	src, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	in, err := os.Open(src)
	if err != nil {
		t.Fatal(err)
	}
	defer in.Close()
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, 0o755)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := io.Copy(out, in); err != nil {
		out.Close()
		t.Fatal(err)
	}
	if err := out.Close(); err != nil {
		t.Fatal(err)
	}
}
