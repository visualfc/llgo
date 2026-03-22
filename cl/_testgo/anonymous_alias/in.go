// LITTEST
package main

type MyPoint = struct {
	x float64
	y float64
}

type IPoint = struct {
	x float64
	y float64
}
type S struct {
}

type I interface {
	NewPoint(dx, dy float64) *IPoint
}

func (S) NewPoint(dx, dy float64) *MyPoint {
	p := &MyPoint{}
	p.x += dx
	p.y += dy
	return p
}

func main() {
	var s I = S{}
	pt := s.NewPoint(float64(3), float64(4))
	println(pt.x, pt.y)
}
