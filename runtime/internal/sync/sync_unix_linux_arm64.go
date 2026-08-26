//go:build linux && arm64

package sync

const (
	pthreadOnceSize      = 4
	pthreadMutexSize     = 48
	pthreadMutexAttrSize = 8
	pthreadCondSize      = 48
	pthreadCondAttrSize  = 8
)
