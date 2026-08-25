//go:build !windows

package runtime

import "github.com/xgo-dev/llgo/runtime/internal/thread"

type gLifecycleDestructor = thread.KeyDestructor
