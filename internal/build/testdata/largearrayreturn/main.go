package main

const size = 128*1024 + 1

type params struct{ x, y, z int }

func main() {
	a := f(1, 99)
	b := g(size-1, 98)
	c := h(size-1, 98)
	d := withAggregateParam(params{1, 2, 3})
	println(a[1], b[1], c[1], a[size-1], b[size-1], c[size-1], d[0])
	println(f(1, 97)[1])
}

//go:noinline
func f(i, y int) (a [size]byte) {
	a[i] = byte(y)
	return
}

//go:noinline
func g(i, y int) [size]byte {
	var a [size]byte
	a[i] = byte(y)
	return a
}

//go:noinline
func h(i, y int) (a [size]byte) {
	a[i] = byte(y)
	return a
}

//go:noinline
func withAggregateParam(p params) (a [size]byte) {
	a[0] = byte(p.x + p.y + p.z)
	return
}
