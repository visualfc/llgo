//go:build llgo && wasm && (js || (wasip1 && !llgo.wasi_threads))

package runtime

// The logical main G runs package initialization and the testing harness that
// previously used the host toolchain's process stack. Keep the same five MiB
// headroom as the Emscripten executable stack; ordinary goroutines use the
// smaller wasmcontext default.
const wasmMainStackSize = uintptr(5 << 20)

func (gp *g) RunqueueNext() *g {
	return gp.context.platform.runqNext
}

func (gp *g) SetRunqueueNext(next *g) {
	gp.context.platform.runqNext = next
}

func (gp *g) RunqueueQueued() bool {
	return gp.context.platform.runqQueued
}

func (gp *g) SetRunqueueQueued(queued bool) {
	gp.context.platform.runqQueued = queued
}
