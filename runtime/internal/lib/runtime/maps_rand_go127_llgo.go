//go:build (darwin || linux || wasm || windows) && go1.27

package runtime

import _ "unsafe"

// Go 1.27's internal/runtime/maps calls this runtime entry point while
// initializing the map hash keys.
//
//go:linkname maps_bootstrapRand internal/runtime/maps.bootstrapRand
func maps_bootstrapRand() uint64 {
	return rand()
}
