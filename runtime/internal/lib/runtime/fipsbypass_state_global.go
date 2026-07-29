//go:build go1.26 && (!llgo || baremetal)

package runtime

// Bare-metal runtimes have one execution context and must not introduce a
// native TLS relocation for the locality package cache.
var fipsBypassDepth uint32
