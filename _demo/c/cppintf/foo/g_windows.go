//go:build windows

package foo

import _ "unsafe"

// MSVC and Itanium targets use different C++ symbol mangling. Cross the
// language boundary through the C ABI wrapper while retaining g as a C++
// function in bar.cpp.
//
//go:linkname G C.llgo_cppintf_g
func G(cb *Callback)
