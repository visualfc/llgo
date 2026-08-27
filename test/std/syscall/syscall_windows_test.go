//go:build windows

package syscall_test

import (
	"errors"
	"syscall"
	"testing"
)

func TestWindowsProcessIdentity(t *testing.T) {
	if pid := syscall.Getpid(); pid <= 0 {
		t.Errorf("Getpid = %d, want a positive process ID", pid)
	}
	if ppid := syscall.Getppid(); ppid <= 0 {
		t.Errorf("Getppid = %d, want a positive parent process ID", ppid)
	}
	if uid := syscall.Getuid(); uid != -1 {
		t.Errorf("Getuid = %d, want -1", uid)
	}
	if euid := syscall.Geteuid(); euid != -1 {
		t.Errorf("Geteuid = %d, want -1", euid)
	}
	if gid := syscall.Getgid(); gid != -1 {
		t.Errorf("Getgid = %d, want -1", gid)
	}
	if egid := syscall.Getegid(); egid != -1 {
		t.Errorf("Getegid = %d, want -1", egid)
	}
	groups, err := syscall.Getgroups()
	if len(groups) != 0 || !errors.Is(err, syscall.EWINDOWS) {
		t.Errorf("Getgroups = %v, %v; want empty groups, EWINDOWS", groups, err)
	}
}

func TestWindowsUTF16Conversions(t *testing.T) {
	encoded, err := syscall.UTF16FromString("hello")
	if err != nil {
		t.Fatal(err)
	}
	if len(encoded) != 6 || encoded[len(encoded)-1] != 0 {
		t.Fatalf("UTF16FromString = %v, want hello followed by NUL", encoded)
	}
	if decoded := syscall.UTF16ToString(encoded); decoded != "hello" {
		t.Errorf("UTF16ToString = %q, want hello", decoded)
	}
	if _, err := syscall.UTF16PtrFromString("embedded\x00nul"); err == nil {
		t.Error("UTF16PtrFromString accepted an embedded NUL")
	}
}

func TestWindowsDLLCall(t *testing.T) {
	dll, err := syscall.LoadDLL("kernel32.dll")
	if err != nil {
		t.Fatal(err)
	}
	defer dll.Release()
	getCurrentProcessID, err := dll.FindProc("GetCurrentProcessId")
	if err != nil {
		t.Fatal(err)
	}
	pid, _, _ := getCurrentProcessID.Call()
	if int(pid) != syscall.Getpid() {
		t.Errorf("GetCurrentProcessId = %d, want %d", pid, syscall.Getpid())
	}
}
