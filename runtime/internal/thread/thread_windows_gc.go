//go:build windows && !nogc

package thread

import _ "github.com/xgo-dev/llgo/runtime/internal/clite/bdwgc"

const (
	LLGoFiles   = "_wrap/thread_windows_gc.c"
	LLGoPackage = "link"
)
