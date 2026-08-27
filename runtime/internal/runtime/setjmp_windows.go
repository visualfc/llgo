//go:build windows && !baremetal && !wasm

package runtime

import (
	_ "unsafe"

	c "github.com/xgo-dev/llgo/runtime/internal/clite"
)

// Windows has no signal-mask variant of setjmp. These are logical runtime
// declarations: the SSA builder lowers 386 to _setjmp3 with a zero unwind-
// record count, amd64 to _setjmpex with a nil frame, and arm64 to LLGo's
// ABI-only context entry. amd64 also uses an ABI-only restore because CRT
// longjmp behavior varies by CRT linkage and may invoke the Windows virtual
// unwinder. Go defers are unwound by LLGo itself, so the platform unwinder
// must not run here. A Go or C setjmp wrapper would save the wrapper's frame,
// which no longer exists at longjmp.
//
//go:linkname Sigsetjmp C.setjmp
func Sigsetjmp(env *SigjmpBuf, savemask c.Int) c.Int

//go:linkname Siglongjmp C.longjmp
func Siglongjmp(env *SigjmpBuf, val c.Int)
