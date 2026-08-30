//go:build !windows && (!llgo || nogc || baremetal)

package runtime

import "github.com/xgo-dev/llgo/runtime/internal/thread"

type gLifecycleDestructor = thread.KeyDestructor

func EnableForeignThreadRegistration() {}

func EnterForeignThread() bool {
	return false
}

func ExitForeignThread(registered bool) {}
