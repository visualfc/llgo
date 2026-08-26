//go:build !windows && !((linux || darwin) && (amd64 || arm64))

package sync

const (
	pthreadOnceSize      = 4
	pthreadMutexSize     = 40
	pthreadMutexAttrSize = 4
	pthreadCondSize      = 48
	pthreadCondAttrSize  = 4
)
