//go:build unix

package goroot

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"syscall"
	"testing"
	"time"
)

func configureProcessGroup(cmd *exec.Cmd) {
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
}

func killProcessTree(cmd *exec.Cmd) {
	if cmd.Process == nil {
		return
	}
	_ = syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
}

func resourceMonitoringSupported() bool { return true }

func processGroupRSS(processGroupID int) (uint64, error) {
	output, err := exec.Command("ps", "-axo", "pgid=,rss=").Output()
	if err != nil {
		return 0, err
	}
	var totalKiB uint64
	scanner := bufio.NewScanner(bytes.NewReader(output))
	for scanner.Scan() {
		fields := bytes.Fields(scanner.Bytes())
		if len(fields) != 2 {
			continue
		}
		pgid, err := strconv.Atoi(string(fields[0]))
		if err != nil || pgid != processGroupID {
			continue
		}
		rssKiB, err := strconv.ParseUint(string(fields[1]), 10, 64)
		if err != nil {
			return 0, fmt.Errorf("parse RSS from ps output %q: %w", scanner.Text(), err)
		}
		totalKiB += rssKiB
	}
	if err := scanner.Err(); err != nil {
		return 0, err
	}
	return totalKiB << 10, nil
}

func TestRunProgramWaitDelayCleansDescendant(t *testing.T) {
	guardTestTimeout(t)
	disableSystemMemoryLimits(t)

	oldWaitDelay := runProgramWaitDelay
	runProgramWaitDelay = 100 * time.Millisecond
	t.Cleanup(func() { runProgramWaitDelay = oldWaitDelay })

	for _, exitCode := range []int{0, 7} {
		t.Run(fmt.Sprintf("exit-%d", exitCode), func(t *testing.T) {
			pidFile := filepath.Join(t.TempDir(), "descendant.pid")
			script := `sleep 60 & echo $! > "$1"; exit "$2"`
			_, _, gotExitCode, elapsed, err := runProgram(
				t.TempDir(),
				"/bin/sh",
				os.Environ(),
				5*time.Second,
				"-c", script, "sh", pidFile, strconv.Itoa(exitCode),
			)
			if exitCode == 0 {
				if !errors.Is(err, exec.ErrWaitDelay) {
					t.Fatalf("runProgram error = %v, want exec.ErrWaitDelay", err)
				}
			} else {
				if err != nil {
					t.Fatalf("runProgram error = %v, want nil ExitError wrapper", err)
				}
				if gotExitCode != exitCode {
					t.Fatalf("exit code = %d, want %d", gotExitCode, exitCode)
				}
			}
			if elapsed >= 5*time.Second {
				t.Fatalf("runProgram took %s, want bounded WaitDelay return", elapsed)
			}

			pidBytes, readErr := os.ReadFile(pidFile)
			if readErr != nil {
				t.Fatal(readErr)
			}
			pid, parseErr := strconv.Atoi(string(bytes.TrimSpace(pidBytes)))
			if parseErr != nil {
				t.Fatalf("parse descendant PID %q: %v", pidBytes, parseErr)
			}
			t.Cleanup(func() { _ = syscall.Kill(pid, syscall.SIGKILL) })

			deadline := time.Now().Add(2 * time.Second)
			for {
				probeErr := syscall.Kill(pid, 0)
				if errors.Is(probeErr, syscall.ESRCH) {
					break
				}
				if probeErr != nil {
					t.Fatalf("probe descendant %d: %v", pid, probeErr)
				}
				if time.Now().After(deadline) {
					t.Fatalf("descendant %d is still running after runProgram returned", pid)
				}
				time.Sleep(10 * time.Millisecond)
			}
		})
	}
}
