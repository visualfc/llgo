package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/py/math"
)

func main() {
	pi := math.Pi.Float64()
	if !(pi >= 3.14159265358 && pi <= 3.14159265360) {
		panic("unexpected math.pi value")
	}
	c.Printf(c.Str("pi = %f\n"), pi)
}
