//go:build linux
// +build linux

package gotest

import (
	"testing"

	"golang.org/x/sys/unix"
)

func TestKubeXSysUnixNoErrorSyscalls(t *testing.T) {
	rawPID, _ := unix.RawSyscallNoError(unix.SYS_GETPID, 0, 0, 0)
	if rawPID == 0 {
		t.Fatal("RawSyscallNoError(SYS_GETPID) returned zero pid")
	}

	sysPID, _ := unix.SyscallNoError(unix.SYS_GETPID, 0, 0, 0)
	if sysPID != rawPID {
		t.Fatalf("SyscallNoError pid = %d, want %d", sysPID, rawPID)
	}
}

func TestXSysUnixAuxv(t *testing.T) {
	auxv, err := unix.Auxv()
	if err != nil {
		t.Fatal(err)
	}
	const atPagesz = 6
	for _, entry := range auxv {
		if entry[0] == atPagesz {
			if got, want := entry[1], uintptr(unix.Getpagesize()); got != want {
				t.Fatalf("AT_PAGESZ = %d, want %d", got, want)
			}
			return
		}
	}
	t.Fatal("auxv does not contain AT_PAGESZ")
}
