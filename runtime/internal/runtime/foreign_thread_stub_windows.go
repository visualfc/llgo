//go:build windows && (!llgo || nogc || baremetal)

package runtime

import "github.com/xgo-dev/llgo/runtime/internal/thread"

type gLifecycleDestructor = thread.KeyDestructor

// Windows callback bridges still reference this API in configurations where
// BDWGC thread registration is unavailable. Keep the no-op API Windows-only so
// unrelated targets do not retain unused entries in their PCLN metadata.
func EnterForeignThread() bool {
	return false
}

func ExitForeignThread(registered bool) {}
