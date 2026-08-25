//go:build darwin && arm64

package sync

const (
	pthreadOnceSize      = 16
	pthreadMutexSize     = 64
	pthreadMutexAttrSize = 16
	pthreadCondSize      = 48
	pthreadCondAttrSize  = 16
)
