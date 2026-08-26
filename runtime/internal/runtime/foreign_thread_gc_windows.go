//go:build llgo && windows && !nogc && !baremetal

package runtime

import (
	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	"github.com/xgo-dev/llgo/runtime/internal/clite/bdwgc"
	"github.com/xgo-dev/llgo/runtime/internal/thread"
)

// foreignThreadGCRegistrationOwned is set only when LLGo explicitly registers
// the current foreign thread. Keep that registration until the existing G
// lifecycle FLS destructor runs, so repeated callbacks on one host thread do
// not take the collector lock to register and unregister on every entry.
//
//llgo:tls
var foreignThreadGCRegistrationOwned bool

// gLifecycleDestructor selects a wrapper only for Windows GC builds. Collector
// teardown must happen when the FLS lifecycle ends, not at every destroyG call.
func gLifecycleDestructor(_ thread.KeyDestructor) thread.KeyDestructor {
	return destroyWindowsG
}

func destroyWindowsG(ptr c.Pointer) {
	destroyG(ptr)
	// This is the last Go operation in the FLS destructor. A foreign thread
	// that entered through a callback can now leave the collector safely.
	if currentGHasLifecycle {
		releaseForeignThreadRegistration()
	}
}

// EnterForeignThread makes a thread created outside the LLGo runtime visible
// to the collector before it allocates or manipulates Go pointers. The return
// value records whether this call installed the retained registration.
func EnterForeignThread() bool {
	if foreignThreadGCRegistrationOwned {
		return false
	}
	// Runtime-created threads are registered by GC_CreateThread. Their G has
	// no FLS lifecycle because mexit/GC_ExitThread owns teardown.
	if currentG != 0 && !currentGHasLifecycle {
		return false
	}
	if bdwgc.ThreadIsRegistered() != 0 {
		return false
	}
	var base bdwgc.StackBase
	if bdwgc.GetStackBase(&base) != bdwgc.Success {
		panic("runtime: failed to discover foreign thread stack")
	}
	switch status := bdwgc.RegisterMyThread(&base); status {
	case bdwgc.Success:
		foreignThreadGCRegistrationOwned = true
		ready := false
		defer func() {
			if !ready {
				releaseForeignThreadRegistration()
			}
		}()
		// Ensure the existing G lifecycle key will release the manual GC
		// registration when this host thread exits.
		getg()
		ready = true
		return true
	case bdwgc.Duplicate:
		return false
	default:
		panic("runtime: failed to register foreign thread")
	}
}

// ExitForeignThread retains an LLGo-owned registration when the current G has
// an FLS lifecycle. The lifecycle destructor releases it after its last Go/GC
// operation. The fallback covers an entry that could not install a lifecycle.
func ExitForeignThread(registered bool) {
	if registered && !currentGUsesLifecycle() {
		releaseForeignThreadRegistration()
	}
}

func releaseForeignThreadRegistration() {
	if foreignThreadGCRegistrationOwned {
		foreignThreadGCRegistrationOwned = false
		bdwgc.UnregisterMyThread()
	}
}
