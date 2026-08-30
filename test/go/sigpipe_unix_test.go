//go:build unix

package gotest

import (
	"errors"
	"os"
	"os/exec"
	"os/signal"
	"syscall"
	"testing"
)

func TestBrokenPipeReturnsEPIPE(t *testing.T) {
	read, write, err := os.Pipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := read.Close(); err != nil {
		t.Fatal(err)
	}
	defer write.Close()

	if _, err := write.Write([]byte("x")); !errors.Is(err, syscall.EPIPE) {
		t.Fatalf("write to broken pipe error = %v, want EPIPE", err)
	}
}

func TestStdoutBrokenPipeExitsWithSIGPIPE(t *testing.T) {
	const helperEnv = "LLGO_SIGPIPE_STDOUT_HELPER"
	if os.Getenv(helperEnv) == "1" {
		// The test parent ignores SIGPIPE and the child inherits that setting.
		// Exercise Reset so the helper matches a program started with the
		// default disposition before it writes to stdout.
		notify := make(chan os.Signal, 1)
		signal.Notify(notify, syscall.SIGPIPE)
		signal.Reset(syscall.SIGPIPE)
		_, _ = os.Stdout.Write([]byte("x"))
		os.Exit(0)
	}

	cmd := exec.Command(os.Args[0], "-test.run=^TestStdoutBrokenPipeExitsWithSIGPIPE$")
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	if err := stdout.Close(); err != nil {
		t.Fatal(err)
	}
	cmd.Env = append(os.Environ(), helperEnv+"=1")
	err = cmd.Run()
	var exitErr *exec.ExitError
	if !errors.As(err, &exitErr) {
		t.Fatalf("stdout broken-pipe child error = %v, want signal exit", err)
	}
	status, ok := exitErr.Sys().(syscall.WaitStatus)
	if !ok || !status.Signaled() || status.Signal() != syscall.SIGPIPE {
		t.Fatalf("stdout broken-pipe child status = %v, want SIGPIPE", exitErr.Sys())
	}
}
