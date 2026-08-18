//go:build windows

package signal_test

import (
	"bufio"
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"syscall"
	"testing"
	"time"
)

const signalHelperEnv = "LLGO_TEST_SIGNAL_HELPER"

var generateConsoleCtrlEvent = syscall.NewLazyDLL("kernel32.dll").NewProc("GenerateConsoleCtrlEvent")

func sendCtrlBreak(t *testing.T, pid int) {
	t.Helper()
	result, _, callErr := generateConsoleCtrlEvent.Call(syscall.CTRL_BREAK_EVENT, uintptr(pid))
	if result == 0 {
		t.Fatalf("GenerateConsoleCtrlEvent: %v", callErr)
	}
}

func runSignalHelper(t *testing.T, mode string, wantExitError bool) {
	t.Helper()
	cmd := exec.Command(os.Args[0], "-test.run=^TestWindowsSignalHelper$")
	cmd.Env = append(os.Environ(), signalHelperEnv+"="+mode)
	cmd.SysProcAttr = &syscall.SysProcAttr{CreationFlags: syscall.CREATE_NEW_PROCESS_GROUP}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}

	ready := make(chan error, 1)
	go func() {
		scanner := bufio.NewScanner(stdout)
		if scanner.Scan() && scanner.Text() == "ready" {
			ready <- nil
			return
		}
		if err := scanner.Err(); err != nil {
			ready <- err
			return
		}
		ready <- fmt.Errorf("signal helper exited before becoming ready")
	}()
	select {
	case err := <-ready:
		if err != nil {
			_ = cmd.Process.Kill()
			_ = cmd.Wait()
			t.Fatalf("signal helper: %v; stderr: %s", err, stderr.String())
		}
	case <-time.After(5 * time.Second):
		_ = cmd.Process.Kill()
		_ = cmd.Wait()
		t.Fatal("timeout waiting for signal helper")
	}

	sendCtrlBreak(t, cmd.Process.Pid)
	err = cmd.Wait()
	if wantExitError {
		if _, ok := err.(*exec.ExitError); !ok {
			t.Fatalf("signal helper Wait = %v, want default signal termination", err)
		}
		return
	}
	if err != nil {
		t.Fatalf("signal helper Wait: %v; stderr: %s", err, stderr.String())
	}
}

func TestWindowsSignalHelper(t *testing.T) {
	mode := os.Getenv(signalHelperEnv)
	if mode == "" {
		return
	}

	switch mode {
	case "notify", "notify-multiple":
		c := make(chan os.Signal, 1)
		if mode == "notify-multiple" {
			signal.Notify(c, os.Interrupt, syscall.SIGTERM)
		} else {
			signal.Notify(c, os.Interrupt)
		}
		fmt.Println("ready")
		select {
		case got := <-c:
			if got != os.Interrupt {
				os.Exit(2)
			}
		case <-time.After(5 * time.Second):
			os.Exit(3)
		}
	case "context":
		ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
		defer stop()
		fmt.Println("ready")
		select {
		case <-ctx.Done():
		case <-time.After(5 * time.Second):
			os.Exit(3)
		}
	case "multiple-channels":
		c1 := make(chan os.Signal, 1)
		c2 := make(chan os.Signal, 1)
		signal.Notify(c1, os.Interrupt)
		signal.Notify(c2, os.Interrupt)
		fmt.Println("ready")
		for c1 != nil || c2 != nil {
			select {
			case <-c1:
				c1 = nil
			case <-c2:
				c2 = nil
			case <-time.After(5 * time.Second):
				os.Exit(3)
			}
		}
	case "stop":
		c := make(chan os.Signal, 1)
		signal.Notify(c, os.Interrupt)
		signal.Stop(c)
		fmt.Println("ready")
		time.Sleep(5 * time.Second)
		os.Exit(4)
	case "reset":
		c := make(chan os.Signal, 1)
		signal.Notify(c, os.Interrupt)
		signal.Reset(os.Interrupt)
		fmt.Println("ready")
		time.Sleep(5 * time.Second)
		os.Exit(4)
	case "reset-all":
		c := make(chan os.Signal, 1)
		signal.Notify(c, os.Interrupt)
		signal.Reset()
		fmt.Println("ready")
		time.Sleep(5 * time.Second)
		os.Exit(4)
	case "ignore":
		signal.Ignore(os.Interrupt)
		fmt.Println("ready")
		time.Sleep(250 * time.Millisecond)
	default:
		os.Exit(5)
	}
	os.Exit(0)
}

func TestNotify(t *testing.T) {
	runSignalHelper(t, "notify", false)
}

func TestNotifyMultipleSignals(t *testing.T) {
	runSignalHelper(t, "notify-multiple", false)
}

func TestStop(t *testing.T) {
	runSignalHelper(t, "stop", true)
}

func TestReset(t *testing.T) {
	runSignalHelper(t, "reset", true)
}

func TestResetAll(t *testing.T) {
	runSignalHelper(t, "reset-all", true)
}

func TestIgnore(t *testing.T) {
	// Go's Windows runtime records os.Interrupt as ignored, but its console
	// handler returns control to Windows when no notification channel wants the
	// event. Windows therefore still applies the default process termination.
	runSignalHelper(t, "ignore", true)
}

func TestIgnored(t *testing.T) {
	wasIgnored := signal.Ignored(os.Interrupt)
	signal.Ignore(os.Interrupt)
	if !signal.Ignored(os.Interrupt) {
		t.Fatal("os.Interrupt is not ignored after Ignore")
	}
	signal.Reset(os.Interrupt)
	if got := signal.Ignored(os.Interrupt); got != wasIgnored {
		t.Logf("Ignored(os.Interrupt) after Reset = %v; before Ignore it was %v", got, wasIgnored)
	}
}

func TestNotifyContext(t *testing.T) {
	runSignalHelper(t, "context", false)
}

func TestNotifyContextStop(t *testing.T) {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	stop()
	select {
	case <-ctx.Done():
	case <-time.After(time.Second):
		t.Fatal("context was not canceled after stop")
	}
}

func TestMultipleChannels(t *testing.T) {
	runSignalHelper(t, "multiple-channels", false)
}
