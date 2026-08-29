//go:build windows && 386

package main

import (
	"github.com/goplus/lib/c"
	multi "github.com/xgo-dev/llgo/_demo/c/cppintf/multi"
)

func multiCallbackCalc() c.Pointer {
	return multi.Windows386CalcThunk()
}

func multiCallbackVal() c.Pointer {
	return multi.Windows386ValThunk()
}

//export llgo_cppmintf_calc_cdecl
func llgo_cppmintf_calc_cdecl(this c.Pointer, value float64) float64 {
	return (*MultiBar)(this).sqrt(value)
}

//export llgo_cppmintf_val_cdecl
func llgo_cppmintf_val_cdecl(this c.Pointer) c.Int {
	return multiIValGetA(this)
}
