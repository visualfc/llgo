//go:build plan9

package os_test

import "syscall"

func openNewFileDescriptor(name string) (uintptr, error) {
	fd, err := syscall.Create(name, syscall.O_RDWR, 0o600)
	return uintptr(fd), err
}
