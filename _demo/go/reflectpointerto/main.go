package main

import (
	"log"
	"reflect"
)

func main() {
	PointerTo()
	AddrOnPointerField()
	PointerToDynamic()
	NamedPointer()
}

func PointerTo() {
	got := reflect.PointerTo(reflect.TypeOf((*int)(nil)))
	want := reflect.TypeOf((**int)(nil))
	if got != want {
		log.Panicf("PointerTo(*int) = %v, want %v\n", got, want)
	}
}

func AddrOnPointerField() {
	type S struct{ N *int }
	v := reflect.ValueOf(&S{}).Elem().Field(0).Addr().Type()
	want := reflect.TypeOf((**int)(nil))
	if v != want {
		log.Panicf("Addr().Type() = %v, want %v\n", v, want)
	}
}

type T struct{}

func PointerToDynamic() {
	t := reflect.TypeOf(T{})
	st := reflect.SliceOf(t)
	s := st.String()
	pst := reflect.PointerTo(st)
	if pst.String() != "*"+s {
		panic(pst.String())
	}
	ppst := reflect.PointerTo(pst)
	if ppst.String() != "**"+s {
		panic(ppst.String())
	}
	pppst := reflect.PointerTo(ppst)
	if pppst.String() != "***"+s {
		panic(pppst.String())
	}
	ppppst := reflect.PointerTo(pppst)
	if ppppst.String() != "****"+s {
		panic(ppppst.String())
	}
}

type Ptr *int

func NamedPointer() {
	t := reflect.TypeOf(Ptr(nil))
	s := t.String()
	if s[0] == '*' {
		panic(s)
	}
	pt := reflect.PointerTo(t)
	if pt.String() != "*"+s {
		panic(pt.String())
	}
	ppt := reflect.PointerTo(pt)
	if ppt.String() != "**"+s {
		panic(ppt.String())
	}
}
