//go:build !llgo

package llgocmd

import (
	"bytes"
	"os"
	"os/exec"
	"testing"
)

const toolCompilerName = "go"

func runToolCompile(t *testing.T, dir string, args ...string) (string, error) {
	t.Helper()
	cmd := exec.Command("go", append([]string{"tool", "compile"}, args...)...)
	cmd.Dir = dir
	cmd.Env = os.Environ()
	output, err := cmd.CombinedOutput()
	return string(output), err
}

func runCompiler(t *testing.T, dir string, args ...string) (string, error) {
	t.Helper()
	cmd := exec.Command("go", args...)
	cmd.Dir = dir
	cmd.Env = os.Environ()
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err := cmd.Run()
	if err != nil {
		stdout.Write(stderr.Bytes())
	}
	return stdout.String(), err
}
