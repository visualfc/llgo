//go:build windows

package main

import (
	"os"
	"os/exec"

	"github.com/goplus/lib/c"
	cos "github.com/goplus/lib/c/os"
)

func runCExecChild() {
	command := c.Str("cmd.exe")
	if cos.Execlp(command, command, c.Str("/d"), c.Str("/c"), c.Str("echo LLGO_C_EXECLP_OK"), nil) == -1 {
		panic("C _execlp failed")
	}
}

// syscall.Exec is not available on Windows. Keep the same controlled-child
// contract there while the Unix source exercises the Go process-replacement
// syscall itself.
func runGoExecChild() {
	command := exec.Command("cmd.exe", "/d", "/c", "echo", goExecOutput)
	command.Stdout = os.Stdout
	command.Stderr = os.Stderr
	if err := command.Run(); err != nil {
		panic(err)
	}
	os.Exit(0)
}
