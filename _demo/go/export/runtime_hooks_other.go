//go:build !linux

package main

//export AllThreadsSyscallStatus
func AllThreadsSyscallStatus() int {
	return 0
}
