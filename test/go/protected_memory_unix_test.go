//go:build linux || darwin

package gotest

import (
	"syscall"
	"testing"
)

func protectedMemory(t *testing.T, totalPages, protectedPage, protectedPages int) ([]byte, int) {
	t.Helper()
	pageSize := syscall.Getpagesize()
	data, err := syscall.Mmap(
		-1,
		0,
		totalPages*pageSize,
		syscall.PROT_READ|syscall.PROT_WRITE,
		syscall.MAP_ANON|syscall.MAP_PRIVATE,
	)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() {
		if err := syscall.Munmap(data); err != nil {
			t.Errorf("munmap protected test memory: %v", err)
		}
	})
	start := protectedPage * pageSize
	end := start + protectedPages*pageSize
	if err := syscall.Mprotect(data[start:end], syscall.PROT_NONE); err != nil {
		t.Fatal(err)
	}
	return data, pageSize
}
