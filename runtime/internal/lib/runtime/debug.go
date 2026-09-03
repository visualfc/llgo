package runtime

import llruntime "github.com/xgo-dev/llgo/runtime/internal/runtime"

func NumCPU() int {
	return int(c_maxprocs())
}

func Breakpoint() {
	c_debugtrap()
}

func Gosched() {
	llruntime.Gosched()
}

func NumCgoCall() int64 {
	return 0
}
