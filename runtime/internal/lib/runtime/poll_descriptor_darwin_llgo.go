//go:build darwin && !baremetal

package runtime

import c "github.com/xgo-dev/llgo/runtime/internal/clite"

func pollDescriptorUnsupported(c.Int) bool {
	// os.newFile already applies the kqueue-specific regular-file, directory,
	// and FIFO exclusions before it initializes internal/poll on Darwin.
	return false
}
