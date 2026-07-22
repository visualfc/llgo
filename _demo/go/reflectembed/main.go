package main

import (
	"fmt"
	"reflect"
)

func main() {
	p1 := Point1{&Point{10, 20}}
	testv(p1)
	testv(&p1)
	p2 := Point2{&Point{10, 20}, 0}
	testv(p2)
	testv(&p2)
	p3 := &Point3{Point{10, 20}}
	testp(p3)
}

func testv(a any) {
	v := reflect.ValueOf(a)
	if v.Kind() == reflect.Ptr {
		v = v.Elem()
	}
	m := v.MethodByName("Set")
	m.Call([]reflect.Value{reflect.ValueOf(100), reflect.ValueOf(200)})
	if fmt.Sprint(v.Interface()) != "(100,200)" {
		panic(fmt.Errorf("error: %#v", a))
	}
}

func testp(a any) {
	v := reflect.ValueOf(a)
	m := v.MethodByName("Set")
	m.Call([]reflect.Value{reflect.ValueOf(100), reflect.ValueOf(200)})
	if fmt.Sprint(v.Interface()) != "(100,200)" {
		panic(fmt.Errorf("error: %#v", a))
	}
}

type Point struct {
	X int
	Y int
}

func (i Point) String() string {
	return fmt.Sprintf("(%v,%v)", i.X, i.Y)
}

func (i *Point) Set(x int, y int) {
	i.X, i.Y = x, y
}

type Point1 struct {
	*Point
}

type Point2 struct {
	*Point
	n int
}

type Point3 struct {
	Point
}
