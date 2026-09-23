//go:build llgo && js && wasm && llgo.wasm.workers

package runtime

// Worker callbacks are queued by the Emscripten host bridge. The scheduler
// dispatches each one on an ordinary G pinned to its originating worker.
func HandleWasmEvent(handler func()) { handler() }

func PollWasmEvent() {}
