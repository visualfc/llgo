//go:build !plan9

package os_test

import "syscall"

func openNewFileDescriptor(name string) (uintptr, error) {
	fd, err := syscall.Open(name, syscall.O_RDWR|syscall.O_CREAT|syscall.O_TRUNC, 0o600)
	return uintptr(fd), err
}
