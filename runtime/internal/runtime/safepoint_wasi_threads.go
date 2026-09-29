//go:build llgo && wasip1 && wasm && llgo.wasi_threads && llgo.wasm.gc.linear

package runtime

// Match the bounded compiler-poll quantum used by the browser schedulers.
const wasiSafepointQuantum = uint32(1024)

//llgointernal:tls
var wasiSafepointBudget uint32

// CooperativeSafepoint is inserted at Go function entries and loop backs.
// Amortize the host mutex check over a bounded number of these polls. Besides
// reducing interpreter overhead, this gives resumed Go code a progress window
// when another pthread repeatedly forces a full stop-the-world collection.
// Allocator waits still call wasiGCSafepoint directly; blocking runtime waits
// publish their roots before entering C.
func CooperativeSafepoint() {
	if wasiSafepointBudget == 0 {
		wasiGCSafepoint()
		wasiSafepointBudget = wasiSafepointQuantum
	}
	wasiSafepointBudget--
}
