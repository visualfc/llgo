//go:build !windows

package main

import (
	"os"
	"os/exec"
	"syscall"

	"github.com/goplus/lib/c"
	cos "github.com/goplus/lib/c/os"
)

func echoPath() string {
	path, err := exec.LookPath("echo")
	if err != nil {
		panic(err)
	}
	return path
}

func runCExecChild() {
	command := c.Str("echo")
	if cos.Execlp(command, command, c.Str("LLGO_C_EXECLP_OK"), nil) == -1 {
		panic("C execlp failed")
	}
}

func runGoExecChild() {
	if err := syscall.Exec(echoPath(), []string{"echo", goExecOutput}, os.Environ()); err != nil {
		panic(err)
	}
}
