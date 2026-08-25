//go:build darwin && amd64

package sync

const (
	pthreadOnceSize      = 16
	pthreadMutexSize     = 64
	pthreadMutexAttrSize = 8
	pthreadCondSize      = 40
	pthreadCondAttrSize  = 8
)
