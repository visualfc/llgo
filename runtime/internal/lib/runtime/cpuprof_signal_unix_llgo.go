//go:build !baremetal && !wasm && (darwin || linux) && (amd64 || arm64)

package runtime

import csyscall "github.com/xgo-dev/llgo/runtime/internal/clite/syscall"

const cpuProfileSignal = uint32(csyscall.SIGPROF)
