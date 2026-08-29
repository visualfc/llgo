package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

const (
	processChildEnv = "LLGO_DEMO_PROCESS_CHILD"
	cExecChild      = "c-execlp"
	goExecChild     = "go-syscall-exec"
	cExecOutput     = "LLGO_C_EXECLP_OK"
	goExecOutput    = "LLGO_GO_EXEC_OK"
)

// One process case covers executable lookup, Cmd.Dir, buffered stdout and the
// Run/Output paths without replacing the test process.
func main() {
	switch os.Getenv(processChildEnv) {
	case cExecChild:
		runCExecChild()
		panic("C execlp returned")
	case goExecChild:
		runGoExecChild()
		panic("Go process replacement returned")
	}

	goTool, err := exec.LookPath("go")
	if err != nil {
		panic(err)
	}
	dir, err := os.MkdirTemp("", "llgo-process-*")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(dir)

	cmd := exec.Command(goTool, "env", "GOOS")
	cmd.Dir = filepath.Clean(dir)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	if err := cmd.Run(); err != nil {
		panic(err)
	}
	if got := strings.TrimSpace(stdout.String()); got != runtime.GOOS {
		panic("unexpected GOOS: " + got)
	}

	out, err := exec.Command(goTool, "env", "GOARCH").Output()
	if err != nil {
		panic(err)
	}
	if got := strings.TrimSpace(string(out)); got != runtime.GOARCH {
		panic("unexpected GOARCH: " + got)
	}

	executable, err := os.Executable()
	if err != nil {
		panic(err)
	}
	for _, test := range []struct {
		child string
		want  string
	}{
		{cExecChild, cExecOutput},
		{goExecChild, goExecOutput},
	} {
		child := exec.Command(executable)
		child.Env = append(os.Environ(), processChildEnv+"="+test.child)
		output, err := child.Output()
		if err != nil {
			panic(fmt.Sprintf("%s child: %v", test.child, err))
		}
		if got := strings.TrimSpace(string(output)); got != test.want {
			panic(fmt.Sprintf("%s child output: got %q, want %q", test.child, got, test.want))
		}
	}
	println("process ok")
}
