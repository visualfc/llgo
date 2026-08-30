//go:build windows && 386

package foo

import "unsafe"

//go:linkname Windows386ValThunk C.llgo_cppintf_val_thunk
func Windows386ValThunk() unsafe.Pointer

//go:linkname Windows386CalcThunk C.llgo_cppintf_calc_thunk
func Windows386CalcThunk() unsafe.Pointer
