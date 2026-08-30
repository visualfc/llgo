//go:build windows && 386

package multi

import "unsafe"

//go:linkname Windows386CalcThunk C.llgo_cppmintf_calc_thunk
func Windows386CalcThunk() unsafe.Pointer

//go:linkname Windows386ValThunk C.llgo_cppmintf_val_thunk
func Windows386ValThunk() unsafe.Pointer
