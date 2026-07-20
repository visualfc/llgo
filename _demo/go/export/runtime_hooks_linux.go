//go:build linux

package main

import "syscall"

func gettimeofdayStatus() int {
	var tv syscall.Timeval
	if err := syscall.Gettimeofday(&tv); err != nil {
		if errno, ok := err.(syscall.Errno); ok {
			return int(errno)
		}
		return int(syscall.EINVAL)
	}
	if tv.Sec <= 0 {
		return int(syscall.EINVAL)
	}
	return 0
}

//export AllThreadsSyscallStatus
func AllThreadsSyscallStatus() int {
	if status := gettimeofdayStatus(); status != 0 {
		return status
	}
	_, _, err := syscall.AllThreadsSyscall(syscall.SYS_GETPID, 0, 0, 0)
	return int(err)
}
