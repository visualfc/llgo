/*
 * Copyright (c) 2024 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package runtime

import (
	"unsafe"

	c "github.com/goplus/llgo/runtime/internal/clite"
	"github.com/goplus/llgo/runtime/internal/clite/setjmp"
)

// -----------------------------------------------------------------------------

// Defer presents defer statements in a function.
type Defer struct {
	Addr unsafe.Pointer // sigjmpbuf
	Bits uintptr
	Link *Defer
	Reth unsafe.Pointer // native block address or wasm continuation selector
	Rund unsafe.Pointer // native block address or wasm continuation selector
	Args unsafe.Pointer // defer func and args links
}

// Recover recovers a panic.
func Recover() (ret any) {
	gp := getg()
	ptr := gp.panic_
	if ptr != nil {
		gp.panic_ = nil
		ret = *(*any)(ptr)
		c.Free(ptr)
		if PanicRecovered != nil {
			PanicRecovered()
		}
		// The deferred function that recovers keeps observing the panic
		// stack until it returns (gc runs defers on top of it). The public
		// runtime marks its frame so the pc snapshot stays spliceable that
		// long; the mark reads the frame-pointer chain, which after
		// siglongjmp can reach a stale/unmapped slot, so the guarded read
		// lives in the package that has a page probe (RecoverMark). Nil
		// when lib/runtime is not linked — no snapshot machinery, nothing
		// to mark.
		if RecoverMark != nil {
			RecoverMark()
		}
	}
	return
}

// RecoverMark, set by the public runtime package, records the recovering
// frame for panic-snapshot splicing.
var RecoverMark func()

const (
	// LLGoFiles: the frame-pointer helper must live in the runtime core —
	// programs that never import "runtime" still link Recover.
	LLGoFiles = "_wrap/fp.c"
)

//go:linkname c_framepointer C.llgo_framepointer
func c_framepointer() unsafe.Pointer

// Panic panics with a value.
func Panic(v any) {
	if v == nil {
		v = &PanicNilError{}
	}
	SavePanicCallerFrames()
	ptr := c.Malloc(unsafe.Sizeof(v))
	*(*any)(ptr) = v
	gp := getg()
	gp.panic_ = ptr

	Rethrow(gp.defer_)
}

func Goexit() {
	gp := getg()
	gp.goexit = true
	Rethrow(gp.defer_)
}

func init() {
	getg().isMain = true
}

// -----------------------------------------------------------------------------

// TracePanic prints panic message.
func TracePanic(v any) {
	print("panic: ")
	printany(v)
	println("\n")
}

// PanicTraceback, when set by the public runtime package, prints a
// Go-style stack trace for an unrecovered panic and reports whether it
// printed anything; the clite frame dump remains the fallback.
var PanicTraceback func(skip int) bool

// PanicRecovered, when set by the public runtime package, releases auxiliary
// traceback state associated with a recovered panic.
var PanicRecovered func()

// PanicSignal converts a hardware signal into the same Go panic the
// legacy signal handler raised.
func PanicSignal(sig int) {
	switch sig {
	case 8: // SIGFPE
		panic(errorString("integer divide by zero"))
	default: // SIGSEGV, SIGBUS
		panic(errorString("invalid memory address or nil pointer dereference"))
	}
}

/*
func stringTracef(fp c.FilePtr, format *c.Char, s String) {
	cs := c.Alloca(uintptr(s.len) + 1)
	c.Fprintf(fp, format, CStrCopy(cs, s))
}
*/

// -----------------------------------------------------------------------------

// New allocates memory and initializes it to zero.
func New(t *Type) unsafe.Pointer {
	return AllocZ(t.Size_)
}

// NewArray allocates memory for an array and initializes it to zero.
func NewArray(t *Type, n int) unsafe.Pointer {
	return AllocZ(uintptr(n) * t.Size_)
}

// -----------------------------------------------------------------------------

// TODO(xsw): check this
// must match declarations in runtime/map.go.
const MaxZero = 1024

var ZeroVal [MaxZero]byte

// -----------------------------------------------------------------------------

type SigjmpBuf struct {
	Unused [setjmp.SigjmpBufSize]byte
}

// -----------------------------------------------------------------------------
