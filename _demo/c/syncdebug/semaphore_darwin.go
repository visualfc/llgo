//go:build darwin

package main

import (
	"github.com/goplus/lib/c"
	cos "github.com/goplus/lib/c/os"
	csyscall "github.com/goplus/lib/c/syscall"
)

func initWorkerSemaphore() bool {
	// Darwin declares unnamed POSIX semaphores for source compatibility, but
	// sem_init always fails with ENOSYS. Verify that platform contract while
	// mutex, condition-variable, and pthread synchronization remain exercised.
	return workerSem.Init(0, 0) == -1 && cos.Errno() == c.Int(csyscall.ENOSYS)
}

// No semaphore was created after the expected ENOSYS result above. The
// condition variable still performs the worker handoff on Darwin.
func postWorkerSemaphore() bool    { return true }
func waitWorkerSemaphore() bool    { return true }
func destroyWorkerSemaphore() bool { return true }
