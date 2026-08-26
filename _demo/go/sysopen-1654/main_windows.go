package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"syscall"
)

// Regression test for syscall.Open failure on Windows. CreateFile with
// CREATE_NEW must return InvalidHandle and ERROR_FILE_EXISTS for an existing
// path.
func main() {
	path := filepath.Join(os.TempDir(), fmt.Sprintf("sysopen-1654-%d.tmp", os.Getpid()))
	if err := os.WriteFile(path, []byte("x"), 0o600); err != nil {
		panic(fmt.Sprintf("prepare temp file failed: %v", err))
	}
	defer os.Remove(path)

	fd, err := syscall.Open(path, syscall.O_CREAT|syscall.O_EXCL|syscall.O_RDWR, 0o600)
	if err == nil {
		panic(fmt.Sprintf("unexpected nil error: fd=%#x", uintptr(fd)))
	}
	if fd != syscall.InvalidHandle {
		panic(fmt.Sprintf("unexpected handle on failure: got=%#x want=%#x err=%v", uintptr(fd), uintptr(syscall.InvalidHandle), err))
	}
	if !errors.Is(err, syscall.ERROR_FILE_EXISTS) {
		panic(fmt.Sprintf("unexpected error: got=%v want=%v", err, syscall.ERROR_FILE_EXISTS))
	}

	fmt.Println("ok")
}
