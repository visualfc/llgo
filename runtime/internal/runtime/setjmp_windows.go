//go:build windows && !baremetal && !wasm

package runtime

import (
	_ "unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

// Windows has no signal-mask variant of setjmp. This is a logical runtime
// declaration: the SSA builder lowers it directly to _setjmp3 or _setjmpex
// and supplies the architecture-specific second operand: a zero unwind-record
// count on 386, or the current frame on amd64 and arm64. A Go or C wrapper
// would save the wrapper's frame, which no longer exists at longjmp.
//
//go:linkname Sigsetjmp C.setjmp
func Sigsetjmp(env *SigjmpBuf, savemask c.Int) c.Int

//go:linkname Siglongjmp C.longjmp
func Siglongjmp(env *SigjmpBuf, val c.Int)
