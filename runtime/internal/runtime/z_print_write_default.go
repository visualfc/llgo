//go:build !windows

package runtime

import c "github.com/xgo-dev/llgo/runtime/internal/clite"

const platformLLGoFiles = ""

func PrintByte(v byte) {
	c.Fputc(c.Int(v), c.Stderr)
}

func PrintString(s String) {
	c.Fwrite(s.data, 1, uintptr(s.len), c.Stderr)
}
