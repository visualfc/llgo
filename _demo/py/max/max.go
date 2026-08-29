package main

import (
	"github.com/goplus/lib/py"
	"github.com/goplus/lib/py/std"
)

func main() {
	x := std.Max(py.Float(3.0), py.Float(9.0), py.Float(23.0), py.Float(100.0))

	list := py.List(3.0, 9.0, 23.0, 100.0)
	y := std.Max(std.Iter(list))
	if x.Float64() != 100 || y.Float64() != 100 {
		panic("unexpected max result")
	}
	std.Print(x)
	std.Print(y)
}
