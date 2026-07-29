//go:build !llgo

package main

import (
	"bytes"
	"os"
	"strings"
	"testing"
)

func TestToolCommand(t *testing.T) {
	cmd := &Cmd_tool{App: new(App)}
	cmd.Main("tool")
	if cmd.Command.Command.Use != "tool [command]" || cmd.Command.Command.Short != "Run a specified llgo tool" {
		t.Fatalf("tool command metadata = (%q, %q)", cmd.Command.Command.Use, cmd.Command.Command.Short)
	}
	if got := cmd.Classfname(); got != "tool" {
		t.Fatalf("Classfname() = %q, want tool", got)
	}
	var output bytes.Buffer
	cmd.SetOut(&output)
	cmd.SetErr(&output)
	if cmd.Run == nil {
		t.Fatal("tool command has no run function")
	}
	cmd.Run(&cmd.Command.Command, nil)
	if !strings.Contains(output.String(), "tool [command]") {
		t.Fatalf("tool help output = %q", output.String())
	}
}

func TestLLDBCommand(t *testing.T) {
	cmd := &Cmd_lldb{App: new(App)}
	cmd.Main("lldb")
	if cmd.Command.Command.Use != "lldb [-lldb path] [--] executable [lldb arguments...]" ||
		cmd.Command.Command.Short != "Debug an LLGo executable with LLDB" {
		t.Fatalf("lldb command metadata = (%q, %q)", cmd.Command.Command.Use, cmd.Command.Command.Short)
	}
	if !cmd.DisableFlagParsing {
		t.Fatal("lldb should pass flags through unchanged")
	}
	if got := cmd.Classfname(); got != "lldb" {
		t.Fatalf("Classfname() = %q, want lldb", got)
	}
	if cmd.Run == nil {
		t.Fatal("lldb command has no run function")
	}
}

func TestToolCompileCommand(t *testing.T) {
	cmd := &Cmd_tool_compile{App: new(App)}
	cmd.Main("compile")
	if cmd.Command.Command.Use != "compile [options] file.go..." || cmd.Command.Command.Short != "Compile Go source files without linking" {
		t.Fatalf("compile command metadata = (%q, %q)", cmd.Command.Command.Use, cmd.Command.Command.Short)
	}
	if !cmd.DisableFlagParsing {
		t.Fatal("tool compile should pass flags through unchanged")
	}
	if got := cmd.Classfname(); got != "tool_compile" {
		t.Fatalf("Classfname() = %q, want tool_compile", got)
	}
	if cmd.Run == nil {
		t.Fatal("tool compile command has no run function")
	}
	output := captureToolStdout(t, func() {
		cmd.Run(&cmd.Command.Command, []string{"-V"})
	})
	if !strings.Contains(output, "compile version llgo") {
		t.Fatalf("tool compile version output = %q", output)
	}
}

func captureToolStdout(t *testing.T, fn func()) string {
	t.Helper()
	file, err := os.CreateTemp(t.TempDir(), "stdout")
	if err != nil {
		t.Fatal(err)
	}
	old := os.Stdout
	os.Stdout = file
	defer func() { os.Stdout = old }()
	fn()
	if err := file.Close(); err != nil {
		t.Fatal(err)
	}
	data, err := os.ReadFile(file.Name())
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}
