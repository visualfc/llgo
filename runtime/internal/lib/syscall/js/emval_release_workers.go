//go:build llgo && js && wasm && llgo.wasm.workers

package js

import (
	"sync"

	llruntime "github.com/xgo-dev/llgo/runtime/internal/runtime"
)

type pendingEmvalRelease struct {
	next   *pendingEmvalRelease
	handle uintptr
	owner  int
}

var pendingEmvalReleases struct {
	sync.Mutex
	head *pendingEmvalRelease
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
	pendingEmvalReleases.Lock()
	release.next = pendingEmvalReleases.head
	pendingEmvalReleases.head = release
	pendingEmvalReleases.Unlock()

	// Install the callback poll even if the program never created a js.Func.
	// The finalizer goroutine may belong to any scheduler worker, whereas an
	// Emscripten emval handle belongs to the JavaScript realm that created it.
	ensureCallbackPoll()
	llruntime.WakeWasmCallbackPoll()
}

func pollEmvalReleases() {
	owner := llruntime.SchedulerProcID()
	var ready *pendingEmvalRelease
	pendingEmvalReleases.Lock()
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
	pendingEmvalReleases.Unlock()

	for ready != nil {
		release := ready
		ready = release.next
		release.next = nil
		cEmvalDecref(release.handle)
	}
}
