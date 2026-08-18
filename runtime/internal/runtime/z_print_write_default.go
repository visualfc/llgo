//go:build !windows

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

const platformLLGoFiles = ""

func printWrite(data unsafe.Pointer, size uintptr) {
	c.Fwrite(data, 1, size, c.Stderr)
}
