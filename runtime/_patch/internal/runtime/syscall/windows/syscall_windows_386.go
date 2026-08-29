// Copyright 2026 The XGo Authors (xgo.dev). All rights reserved.
// Use of this source code is governed by a BSD-style license.
// See LICENSES/Go-BSD-3-Clause.txt at this module root for license terms.

//go:build windows && 386

package windows

import (
	"internal/abi"
	_ "unsafe" // for go:linkname
)

// Go's 386 implementation provides these entry points in Plan 9 assembly.
// LLGo's runtime already has an ABI-neutral variable-width Windows call bridge,
// so both the direct call and callback address reuse that implementation.

//go:linkname StdCall runtime.llgoWindowsStdCall
//go:linkname asmstdcall runtime.llgoWindowsStdCall

func AsmStdCallAddr() uintptr {
	return abi.FuncPCABIInternal(asmstdcall)
}
