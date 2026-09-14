//go:build !llgo

package main

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"
)

func TestEnvCommand(t *testing.T) {
	cmd := &Cmd_env{App: new(App)}
	cmd.Main("env")
	if cmd.Command.Command.Use != "env [-json] [-changed] [-target name] [var ...]" || cmd.Classfname() != "env" || !cmd.DisableFlagParsing {
		t.Fatal("env command must pass Go flags and variables through unchanged")
	}
	t.Setenv("GOOS", "wasip1")
	t.Setenv("GOARCH", "wasm")
	output := captureToolStdout(t, func() {
		cmd.Run(&cmd.Command.Command, []string{"GOOS", "GOARCH"})
	})
	if strings.ReplaceAll(output, "\r\n", "\n") != "wasip1\nwasm\n" {
		t.Fatalf("env GOOS GOARCH = %q", output)
	}
}

func TestEnvGoAlias(t *testing.T) {
	if os.Getenv("LLGO_TEST_ENV_CHILD") == "1" {
		os.Args = []string{"go", "env", "GOOS", "GOARCH"}
		main()
		os.Exit(0)
	}
	self, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	dir := t.TempDir()
	name := "go"
	if runtime.GOOS == "windows" {
		name += ".exe"
	}
	// A hard link also exercises file identity on Windows without requiring
	// developer mode or symlink privileges. The resolver uses os.SameFile.
	if err := os.Link(self, filepath.Join(dir, name)); err != nil {
		t.Fatal(err)
	}
	path := os.Getenv("PATH")
	t.Setenv("LLGO_TEST_ENV_CHILD", "1")
	t.Setenv("GOOS", "wasip1")
	t.Setenv("GOARCH", "wasm")
	for _, missing := range []bool{false, true} {
		t.Run(map[bool]string{false: "real Go after alias", true: "only alias"}[missing], func(t *testing.T) {
			search := dir
			if !missing {
				search += string(os.PathListSeparator) + path
			}
			t.Setenv("PATH", search)
			ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
			defer cancel()
			output, err := exec.CommandContext(ctx, self, "-test.run=^TestEnvGoAlias$").CombinedOutput()
			if ctx.Err() != nil {
				t.Fatalf("env recursed or hung: %v", ctx.Err())
			}
			if missing {
				if exit, ok := err.(*exec.ExitError); !ok || exit.ExitCode() != 1 || !strings.Contains(string(output), "Go toolchain not found") {
					t.Fatalf("missing Go: %v, %s", err, output)
				}
			} else if err != nil || strings.ReplaceAll(string(output), "\r\n", "\n") != "wasip1\nwasm\n" {
				t.Fatalf("env through go alias: %v, %s", err, output)
			}
		})
	}
}
