//go:build llgo

package llgoext

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"strings"
	"testing"
	_ "unsafe"
)

const mainGoexitLifecycleChild = "LLGO_TEST_MAIN_GOEXIT_LIFECYCLE"

//go:linkname runtimeGStateForTesting github.com/xgo-dev/llgo/runtime/internal/runtime.GStateForTesting
func runtimeGStateForTesting() (count uint64, mainExited bool)

func init() {
	if os.Getenv(mainGoexitLifecycleChild) != "1" {
		return
	}
	done := make(chan int, 1)
	defer func() { done <- 0 }()
	go func() {
		<-done
		for {
			count, mainExited := runtimeGStateForTesting()
			if count == 1 && mainExited {
				break
			}
		}
		fmt.Println("WORKER_RETURNING")
	}()
	runtime.Goexit()
}

func TestMainGoexitLifecycleReleasedOnce(t *testing.T) {
	cmd := exec.Command(os.Args[0], "-test.run=^$")
	cmd.Env = append(os.Environ(), mainGoexitLifecycleChild+"=1")
	output, err := cmd.CombinedOutput()
	if err == nil {
		t.Fatalf("main Goexit child unexpectedly succeeded:\n%s", output)
	}
	worker := strings.Index(string(output), "WORKER_RETURNING")
	deadlock := strings.Index(string(output), "no goroutines (main called runtime.Goexit) - deadlock!")
	if worker < 0 || deadlock < 0 || worker > deadlock {
		t.Fatalf("worker must return before the last-goroutine deadlock:\n%s", output)
	}
}
