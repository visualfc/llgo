//go:build windows && (nogc || baremetal)

package thread

const (
	LLGoFiles   = "_wrap/thread_windows.c"
	LLGoPackage = "link"
)
