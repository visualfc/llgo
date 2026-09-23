//go:build llgo && js && wasm && llgo.wasm.workers

package runtime

const wasmSafepointQuantum = uint32(1024)

func CooperativeSafepoint() {
	worker := currentWasmWorker()
	if worker == nil || !worker.safepointBudget.Poll() {
		return
	}
	cooperativeSafepointSlow()
}

//go:noinline
func cooperativeSafepointSlow() {
	worker := currentWasmWorker()
	if worker == nil {
		return
	}
	if worker.index == 0 {
		pollWasmEvents()
	}
	if wasmWorkerRunqLen(worker) != 0 {
		goschedBackend()
	}
}
