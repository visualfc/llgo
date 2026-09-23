//go:build llgo && js && wasm && llgo.wasm.workers

package runtime

const wasmSafepointQuantum = uint32(1024)

func CooperativeSafepoint() {
	worker := currentWasmWorker()
	// The collector owns its worker until sweeping has completed. Compiler-
	// inserted polls in GC loops must not reschedule while the allocator and
	// heap metadata are exclusively owned by that worker.
	if worker == nil || wasmGCOwnedBy(worker) ||
		(!wasmGCRequestPending(worker) && !worker.safepointBudget.Poll()) {
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
	if wasmGCRequestPending(worker) {
		if gp := getg(); gp != nil {
			gp.context.platform.context.Swap(
				&worker.system,
				wasmWorkerSystemRootPointer(worker),
			)
		} else {
			wasmWorkerStopForGC(worker)
		}
		return
	}
	if worker.index == 0 {
		pollWasmEvents()
	}
	if wasmWorkerRunqLen(worker) != 0 {
		goschedBackend()
	}
}
