package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/math"
	multi "github.com/xgo-dev/llgo/_demo/c/cppintf/multi"
)

type MultiBar struct {
	multi.Callback
	a c.Int
}

func NewMultiBar(a c.Int) *MultiBar {
	return &MultiBar{
		Callback: multi.Callback{
			ICalc: multi.ICalc{
				Vptr: &multi.ICalcVtbl{
					Calc: multiCallbackCalc(),
				},
			},
			IVal: multi.IVal{
				Vptr: &multi.IValVtbl{
					Val: multiCallbackVal(),
				},
			},
		},
		a: a,
	}
}

func (p *MultiBar) getA() c.Int {
	return p.a
}

func multiIValGetA(this c.Pointer) c.Int {
	const delta = -int(unsafe.Offsetof(multi.Callback{}.IVal))
	return (*MultiBar)(c.Advance(this, delta)).getA()
}

func (p *MultiBar) sqrt(v float64) float64 {
	return math.Sqrt(v)
}

func testMulti() {
	bar := NewMultiBar(1)
	multi.F(&bar.Callback)
}
