//go:build llgo

package llgocmd

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"testing"
)

var (
	llgoOnce sync.Once
	llgoPath string
	llgoErr  string
)

const toolCompilerName = "llgo"

func runToolCompile(t *testing.T, dir string, args ...string) (string, error) {
	t.Helper()
	cmd := exec.Command(testLLGo(t), append([]string{"tool", "compile"}, args...)...)
	cmd.Dir = dir
	cmd.Env = os.Environ()
	output, err := cmd.CombinedOutput()
	return string(output), err
}

func runCompiler(t *testing.T, dir string, args ...string) (string, error) {
	t.Helper()
	cmd := exec.Command(testLLGo(t), args...)
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

func testLLGo(t *testing.T) string {
	t.Helper()
	root := repositoryRoot(t)
	t.Setenv("LLGO_ROOT", root)
	for _, name := range []string{"LLGO_TEST_COMPILER", "LLGO_TEST_LLGO", "LLGO"} {
		if path := os.Getenv(name); path != "" {
			absolute, err := filepath.Abs(path)
			if err != nil {
				t.Fatalf("resolve %s: %v", name, err)
			}
			return absolute
		}
	}

	llgoOnce.Do(func() {
		dir, err := os.MkdirTemp("", "llgo-command-test-")
		if err != nil {
			llgoErr = err.Error()
			return
		}
		llgoPath = executablePath(dir, "llgo")
		cmd := exec.Command("go", "build", "-o", llgoPath, "./cmd/llgo")
		cmd.Dir = root
		if output, err := cmd.CombinedOutput(); err != nil {
			llgoErr = err.Error() + "\n" + string(output)
		}
	})
	if llgoErr != "" {
		t.Fatalf("build llgo: %s", llgoErr)
	}
	return llgoPath
}

func repositoryRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatal("repository root not found")
		}
		dir = parent
	}
}
