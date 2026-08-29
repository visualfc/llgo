package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/py"
	"github.com/goplus/lib/py/math"
	"github.com/goplus/lib/py/statistics"
	"github.com/goplus/lib/py/std"
)

// Canonical Python binding smoke: module values, fixed and variadic calls,
// iterator argument expansion, scalar extraction, Python print and C varargs.
func main() {
	sqrt := math.Sqrt(py.Float(2)).Float64()
	if sqrt < 1.41421356237 || sqrt > 1.41421356238 {
		panic("unexpected math.sqrt result")
	}
	pi := math.Pi.Float64()
	if pi < 3.14159265358 || pi > 3.14159265360 {
		panic("unexpected math.pi value")
	}

	maxArgs := std.Max(py.Float(3), py.Float(9), py.Float(23), py.Float(100))
	list := py.List(3.0, 9.0, 23.0, 100.0)
	maxIter := std.Max(std.Iter(list))
	if maxArgs.Float64() != 100 || maxIter.Float64() != 100 {
		panic("unexpected max result")
	}
	mean := statistics.Mean(py.List(1.0, 2.0, 3.0, 4.0, 4.0)).Float64()
	if mean < 2.799999999 || mean > 2.800000001 {
		panic("unexpected statistics.mean result")
	}

	std.Print(py.Str("sqrt/max ="), math.Sqrt(py.Float(2)), maxArgs, maxIter)
	c.Printf(c.Str("pi/mean = %f/%f\n"), pi, mean)
}
