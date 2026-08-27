//go:build windows

package gotest

import (
	"syscall"
	"testing"
	"unsafe"
)

const (
	windowsMemCommit     = 0x1000
	windowsMemReserve    = 0x2000
	windowsMemRelease    = 0x8000
	windowsPageNoAccess  = 0x01
	windowsPageReadWrite = 0x04
)

func protectedMemory(t *testing.T, totalPages, protectedPage, protectedPages int) ([]byte, int) {
	t.Helper()
	pageSize := syscall.Getpagesize()
	kernel32, err := syscall.LoadDLL("kernel32.dll")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := kernel32.Release(); err != nil {
			t.Errorf("release kernel32.dll: %v", err)
		}
	})
	virtualAlloc, err := kernel32.FindProc("VirtualAlloc")
	if err != nil {
		t.Fatal(err)
	}
	virtualProtect, err := kernel32.FindProc("VirtualProtect")
	if err != nil {
		t.Fatal(err)
	}
	virtualFree, err := kernel32.FindProc("VirtualFree")
	if err != nil {
		t.Fatal(err)
	}

	size := totalPages * pageSize
	address, _, callErr := virtualAlloc.Call(
		0,
		uintptr(size),
		windowsMemCommit|windowsMemReserve,
		windowsPageReadWrite,
	)
	if address == 0 {
		t.Fatalf("VirtualAlloc: %v", callErr)
	}
	t.Cleanup(func() {
		if ok, _, err := virtualFree.Call(address, 0, windowsMemRelease); ok == 0 {
			t.Errorf("VirtualFree: %v", err)
		}
	})

	start := uintptr(protectedPage * pageSize)
	length := uintptr(protectedPages * pageSize)
	var oldProtection uint32
	if ok, _, err := virtualProtect.Call(
		address+start,
		length,
		windowsPageNoAccess,
		uintptr(unsafe.Pointer(&oldProtection)),
	); ok == 0 {
		t.Fatalf("VirtualProtect: %v", err)
	}
	return unsafe.Slice((*byte)(unsafe.Pointer(address)), size), pageSize
}
