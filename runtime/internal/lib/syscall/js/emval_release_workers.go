//go:build llgo && js && wasm && llgo.wasm.workers

package js

import (
	llruntime "github.com/xgo-dev/llgo/runtime/internal/runtime"
	"github.com/xgo-dev/llgo/runtime/internal/wasmsync"
)

type pendingEmvalRelease struct {
	next   *pendingEmvalRelease
	handle uintptr
	owner  int
}

var pendingEmvalReleases struct {
	mutex wasmsync.Mutex
	head  *pendingEmvalRelease
}

func emvalOwner() int {
	return llruntime.SchedulerProcID()
}

func releaseEmval(handle uintptr, owner int) {
	if llruntime.SchedulerProcID() == owner {
		cEmvalDecref(handle)
		return
	}
	release := &pendingEmvalRelease{handle: handle, owner: owner}
	pendingEmvalReleases.mutex.Lock(nil)
	release.next = pendingEmvalReleases.head
	pendingEmvalReleases.head = release
	pendingEmvalReleases.mutex.Unlock()

	// Install the callback poll even if the program never created a js.Func.
	// The finalizer goroutine may belong to any scheduler worker, whereas an
	// Emscripten emval handle belongs to the JavaScript realm that created it.
	ensureCallbackPoll()
	llruntime.WakeWasmCallbackPoll()
}

func pollEmvalReleases() {
	owner := llruntime.SchedulerProcID()
	var ready *pendingEmvalRelease
	pendingEmvalReleases.mutex.Lock(nil)
	link := &pendingEmvalReleases.head
	for *link != nil {
		release := *link
		if release.owner != owner {
			link = &release.next
			continue
		}
		*link = release.next
		release.next = ready
		ready = release
	}
	pendingEmvalReleases.mutex.Unlock()
	if ready != nil {
		// pollEmvalReleases runs on the scheduler's system fiber. Start a G in
		// the same worker realm before touching sync-backed callback state.
		go drainEmvalReleases(ready)
	}
}

func drainEmvalReleases(ready *pendingEmvalRelease) {
	for ready != nil {
		release := ready
		ready = release.next
		release.next = nil
		cEmvalDecref(release.handle)
	}
}
