//go:build windows && 386

package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/xgo-dev/llgo/_demo/c/cppintf/foo"
)

func callbackVal() c.Pointer {
	return foo.Windows386ValThunk()
}

func callbackCalc() c.Pointer {
	return foo.Windows386CalcThunk()
}

// MSVC x86 virtual calls use thiscall, with this in ECX. LLGo's exported Go
// functions use the native cdecl boundary, so bar.cpp supplies the minimal
// thiscall-to-cdecl thunk and forwards the unchanged receiver here.
//
//export llgo_cppintf_val_cdecl
func llgo_cppintf_val_cdecl(this *foo.Callback) c.Int {
	return (*Bar)(unsafe.Pointer(this)).getA()
}

//export llgo_cppintf_calc_cdecl
func llgo_cppintf_calc_cdecl(this *foo.Callback, value float64) float64 {
	return (*Bar)(unsafe.Pointer(this)).sqrt(value)
}
