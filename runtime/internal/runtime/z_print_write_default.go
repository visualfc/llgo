//go:build !windows

package runtime

import (
	"unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

const platformLLGoFiles = ""

func printByte(v byte) {
	c.Fputc(c.Int(v), c.Stderr)
}

func printWrite(data unsafe.Pointer, size uintptr) {
	c.Fwrite(data, 1, size, c.Stderr)
}
