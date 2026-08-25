//go:build linux

package sync

type alignOnce = align4
type alignMutex = alignPtr
type alignCond = alignPtr
type alignMutexAttr = align4
type alignCondAttr = align4
