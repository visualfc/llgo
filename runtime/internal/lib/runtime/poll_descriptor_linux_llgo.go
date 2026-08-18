//go:build linux && !baremetal

package runtime

import (
	c "github.com/xgo-dev/llgo/runtime/internal/clite"
	cliteos "github.com/xgo-dev/llgo/runtime/internal/clite/os"
	csyscall "github.com/xgo-dev/llgo/runtime/internal/clite/syscall"
)

func pollDescriptorUnsupported(fd c.Int) bool {
	var stat cliteos.StatT
	if cliteos.Fstat(fd, &stat) != 0 {
		return false
	}
	fileType := uint32(stat.Mode) & uint32(csyscall.S_IFMT)
	return fileType == uint32(csyscall.S_IFREG) || fileType == uint32(csyscall.S_IFDIR)
}
