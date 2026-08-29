package main

import (
	"github.com/goplus/lib/py"
	"github.com/goplus/lib/py/math"
	"github.com/goplus/lib/py/std"
)

func main() {
	x := math.Sqrt(py.Float(2))
	if got := x.Float64(); !(got >= 1.41421356237 && got <= 1.41421356238) {
		panic("unexpected math.sqrt result")
	}
	std.Print(py.Str("sqrt(2) ="), x)
}
