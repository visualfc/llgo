package main

import "github.com/goplus/lib/c"

type generator struct {
	value c.Int
}

func (g *generator) next() c.Int {
	g.value++
	return g.value
}

func genInts(n int, generate func() c.Int) []c.Int {
	values := make([]c.Int, n)
	for i := range values {
		values[i] = generate()
	}
	return values
}

// Keep the C function value, closure, and method-value generator forms from
// the former standalone genints demo in this C time/random owner.
func verifyGenerators() {
	if values := genInts(5, c.Rand); len(values) != 5 {
		panic("C rand generator")
	}

	value := c.Int(1)
	values := genInts(5, func() c.Int {
		value *= 2
		return value
	})
	if values[0] != 2 || values[4] != 32 {
		panic("closure generator")
	}

	g := &generator{value: 1}
	values = genInts(5, g.next)
	if values[0] != 2 || values[4] != 6 {
		panic("method-value generator")
	}
}
