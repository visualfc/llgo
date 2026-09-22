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

func TestModCommand(t *testing.T) {
	cmd := &Cmd_mod{App: new(App)}
	cmd.Main("mod")
	if cmd.Command.Command.Use != "mod <command> [arguments]" || cmd.Classfname() != "mod" || !cmd.DisableFlagParsing || cmd.Run == nil {
		t.Fatal("mod command must forward Go subcommands and flags unchanged")
	}
}

func TestModGoAlias(t *testing.T) {
	if os.Getenv("LLGO_TEST_MOD_CHILD") == "1" {
		os.Args = []string{"go", "mod", "edit", "-json"}
		if os.Getenv("LLGO_TEST_MOD_INVALID") == "1" {
			os.Args = []string{"go", "mod", "tidy", "-unknown-flag"}
		}
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
	// Hard links exercise alias detection on Windows without symlink privileges.
	alias := filepath.Join(dir, name)
	if err := os.Link(self, alias); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module example.com/alias\n\ngo 1.20\n"), 0644); err != nil {
		t.Fatal(err)
	}
	t.Setenv("LLGO_TEST_MOD_CHILD", "1")
	t.Setenv("GOTOOLCHAIN", "local")
	t.Setenv("GOENV", "off")
	t.Setenv("GOWORK", "off")
	t.Setenv("GOFLAGS", "")
	t.Setenv("PATH", dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	for _, invalid := range []string{"0", "1"} {
		t.Run("invalid="+invalid, func(t *testing.T) {
			t.Setenv("LLGO_TEST_MOD_INVALID", invalid)
			ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
			defer cancel()
			cmd := exec.CommandContext(ctx, alias, "-test.run=^TestModGoAlias$")
			cmd.Dir = dir
			output, err := cmd.CombinedOutput()
			if ctx.Err() != nil {
				t.Fatalf("mod recursed or hung: %v", ctx.Err())
			}
			if invalid == "1" {
				if exit, ok := err.(*exec.ExitError); !ok || exit.ExitCode() != 2 || !strings.Contains(string(output), "flag provided but not defined") {
					t.Fatalf("mod lost Go's failure status/diagnostic: %v, %s", err, output)
				}
			} else if err != nil || !strings.Contains(string(output), `"Path": "example.com/alias"`) {
				t.Fatalf("mod through go alias: %v, %s", err, output)
			}
		})
	}
}
