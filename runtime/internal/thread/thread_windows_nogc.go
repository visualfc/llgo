//go:build windows && nogc

package thread

const (
	LLGoFiles   = "_wrap/thread_windows.c"
	LLGoPackage = "link"
)
