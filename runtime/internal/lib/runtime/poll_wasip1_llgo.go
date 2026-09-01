//go:build wasip1 && wasm

package runtime

import _ "unsafe"

// WASI Preview 1 file descriptors remain usable through their synchronous
// syscall implementation. The single-worker runtime does not yet provide Go's
// poll_oneoff-based asynchronous netpoll contract, so report descriptors as
// unsupported instead of leaving internal/poll hooks as unresolved env imports.

const (
	pollErrNotPollable = 3
	wasiErrnoNotSup    = 58
)

//go:linkname poll_runtime_pollServerInit internal/poll.runtime_pollServerInit
func poll_runtime_pollServerInit() {}

//go:linkname poll_runtime_pollOpen internal/poll.runtime_pollOpen
func poll_runtime_pollOpen(fd uintptr) (uintptr, int) {
	_ = fd
	return 0, wasiErrnoNotSup
}

//go:linkname poll_runtime_pollClose internal/poll.runtime_pollClose
func poll_runtime_pollClose(ctx uintptr) {
	_ = ctx
}

//go:linkname poll_runtime_pollWait internal/poll.runtime_pollWait
func poll_runtime_pollWait(ctx uintptr, mode int) int {
	_, _ = ctx, mode
	return pollErrNotPollable
}

//go:linkname poll_runtime_pollWaitCanceled internal/poll.runtime_pollWaitCanceled
func poll_runtime_pollWaitCanceled(ctx uintptr, mode int) {
	_, _ = ctx, mode
}

//go:linkname poll_runtime_pollReset internal/poll.runtime_pollReset
func poll_runtime_pollReset(ctx uintptr, mode int) int {
	_, _ = ctx, mode
	return pollErrNotPollable
}

//go:linkname poll_runtime_pollSetDeadline internal/poll.runtime_pollSetDeadline
func poll_runtime_pollSetDeadline(ctx uintptr, d int64, mode int) {
	_, _, _ = ctx, d, mode
}

//go:linkname poll_runtime_pollUnblock internal/poll.runtime_pollUnblock
func poll_runtime_pollUnblock(ctx uintptr) {
	_ = ctx
}

//go:linkname poll_runtime_isPollServerDescriptor internal/poll.runtime_isPollServerDescriptor
func poll_runtime_isPollServerDescriptor(fd uintptr) bool {
	_ = fd
	return false
}
