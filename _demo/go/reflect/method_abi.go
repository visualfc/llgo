package main

import . "reflect"

type Tinter interface {
	M(int, byte) (byte, int)
}

type Tsmallv byte

func (v Tsmallv) M(x int, b byte) (byte, int) { return b, x + int(v) }

type Tsmallp byte

func (p *Tsmallp) M(x int, b byte) (byte, int) { return b, x + int(*p) }

type Twordv uintptr

func (v Twordv) M(x int, b byte) (byte, int) { return b, x + int(v) }

type Twordp uintptr

func (p *Twordp) M(x int, b byte) (byte, int) { return b, x + int(*p) }

type Tbigv [2]uintptr

func (v Tbigv) M(x int, b byte) (byte, int) { return b, x + int(v[0]) + int(v[1]) }

type Tbigp [2]uintptr

func (p *Tbigp) M(x int, b byte) (byte, int) { return b, x + int(p[0]) + int(p[1]) }

type tinter interface {
	m(int, byte) (byte, int)
}

// Embedding via pointer.

type Tm1 struct {
	Tm2
}

type Tm2 struct {
	*Tm3
}

type Tm3 struct {
	*Tm4
}

type Tm4 struct {
}

func (t4 Tm4) M(x int, b byte) (byte, int) { return b, x + 40 }

func TestMethod5(t *testingT) {
	CheckF := func(name string, f func(int, byte) (byte, int), inc int) {
		b, x := f(1000, 99)
		if b != 99 || x != 1000+inc {
			t.Errorf("%s(1000, 99) = %v, %v, want 99, %v", name, b, x, 1000+inc)
		}
	}

	CheckV := func(name string, i Value, inc int) {
		bx := i.Method(0).Call([]Value{ValueOf(1000), ValueOf(byte(99))})
		b := bx[0].Interface()
		x := bx[1].Interface()
		if b != byte(99) || x != 1000+inc {
			t.Errorf("direct %s.M(1000, 99) = %v, %v, want 99, %v", name, b, x, 1000+inc)
		}

		CheckF(name+".M", i.Method(0).Interface().(func(int, byte) (byte, int)), inc)
	}

	var TinterType = TypeOf(new(Tinter)).Elem()

	CheckI := func(name string, i any, inc int) {
		v := ValueOf(i)
		CheckV(name, v, inc)
		CheckV("(i="+name+")", v.Convert(TinterType), inc)
	}

	sv := Tsmallv(1)
	CheckI("sv", sv, 1)
	CheckI("&sv", &sv, 1)

	sp := Tsmallp(2)
	CheckI("&sp", &sp, 2)

	wv := Twordv(3)
	CheckI("wv", wv, 3)
	CheckI("&wv", &wv, 3)

	wp := Twordp(4)
	CheckI("&wp", &wp, 4)

	bv := Tbigv([2]uintptr{5, 6})
	CheckI("bv", bv, 11)
	CheckI("&bv", &bv, 11)

	bp := Tbigp([2]uintptr{7, 8})
	CheckI("&bp", &bp, 15)

	t4 := Tm4{}
	t3 := Tm3{&t4}
	t2 := Tm2{&t3}
	t1 := Tm1{t2}
	CheckI("t4", t4, 40)
	CheckI("&t4", &t4, 40)
	CheckI("t3", t3, 40)
	CheckI("&t3", &t3, 40)
	CheckI("t2", t2, 40)
	CheckI("&t2", &t2, 40)
	CheckI("t1", t1, 40)
	CheckI("&t1", &t1, 40)

	var tnil Tinter
	vnil := ValueOf(&tnil).Elem()
	shouldPanic("Method", func() { vnil.Method(0) })
}

// Package-level type definitions for StructResult return type
type StructResult struct{ N byte }

type TinterSmallStruct interface {
	M(int, byte) (StructResult, int)
}

type TsrSmallv byte

func (v TsrSmallv) M(x int, b byte) (StructResult, int) {
	return StructResult{b}, x + int(v)
}

type TsrSmallp byte

func (p *TsrSmallp) M(x int, b byte) (StructResult, int) {
	return StructResult{b}, x + int(*p)
}

type TsrWordv uintptr

func (v TsrWordv) M(x int, b byte) (StructResult, int) {
	return StructResult{b}, x + int(v)
}

type TsrWordp uintptr

func (p *TsrWordp) M(x int, b byte) (StructResult, int) {
	return StructResult{b}, x + int(*p)
}

type TsrBigv [2]uintptr

func (v TsrBigv) M(x int, b byte) (StructResult, int) {
	return StructResult{b}, x + int(v[0]) + int(v[1])
}

type TsrBigp [2]uintptr

func (p *TsrBigp) M(x int, b byte) (StructResult, int) {
	return StructResult{b}, x + int(p[0]) + int(p[1])
}

// Package-level type definitions for [1]byte return type
type TinterSmallArray interface {
	M(int, byte) ([1]byte, int)
}

type TarSmallv byte

func (v TarSmallv) M(x int, b byte) ([1]byte, int) {
	return [1]byte{b}, x + int(v)
}

type TarSmallp byte

func (p *TarSmallp) M(x int, b byte) ([1]byte, int) {
	return [1]byte{b}, x + int(*p)
}

type TarWordv uintptr

func (v TarWordv) M(x int, b byte) ([1]byte, int) {
	return [1]byte{b}, x + int(v)
}

type TarWordp uintptr

func (p *TarWordp) M(x int, b byte) ([1]byte, int) {
	return [1]byte{b}, x + int(*p)
}

type TarBigv [2]uintptr

func (v TarBigv) M(x int, b byte) ([1]byte, int) {
	return [1]byte{b}, x + int(v[0]) + int(v[1])
}

type TarBigp [2]uintptr

func (p *TarBigp) M(x int, b byte) ([1]byte, int) {
	return [1]byte{b}, x + int(p[0]) + int(p[1])
}

// Embedding via pointer for StructResult return type
type TsrEmb1 struct{ TsrEmb2 }
type TsrEmb2 struct{ *TsrEmb3 }
type TsrEmb3 struct{ *TsrEmb4 }
type TsrEmb4 struct{}

func (t4 TsrEmb4) M(x int, b byte) (StructResult, int) {
	return StructResult{b}, x + 40
}

// Embedding via pointer for [1]byte return type
type TarEmb1 struct{ TarEmb2 }
type TarEmb2 struct{ *TarEmb3 }
type TarEmb3 struct{ *TarEmb4 }
type TarEmb4 struct{}

func (t4 TarEmb4) M(x int, b byte) ([1]byte, int) {
	return [1]byte{b}, x + 40
}

// TestMethodSmall is similar to TestMethod5 but tests methods returning small aggregates
func TestMethodSmall(t *testingT) {
	// Helper function: check return value of small struct
	CheckStructF := func(name string, f func(int, byte) (StructResult, int), inc int) {
		ret, x := f(1000, 99)
		if ret.N != byte(99) || x != 1000+inc {
			t.Errorf("%s(1000, 99) = {%v}, %v, want {99}, %v", name, ret.N, x, 1000+inc)
		}
	}

	// Helper function: check via reflection with small struct return
	CheckStructV := func(name string, i Value, inc int) {
		bx := i.Method(0).Call([]Value{ValueOf(1000), ValueOf(byte(99))})
		structVal := bx[0]
		ret := structVal.Field(0).Interface()
		x := bx[1].Interface()
		if ret != byte(99) || x != 1000+inc {
			t.Errorf("direct %s.M(1000, 99) = {%v}, %v, want {99}, %v", name, ret, x, 1000+inc)
		}
		CheckStructF(name+".M", i.Method(0).Interface().(func(int, byte) (StructResult, int)), inc)
	}

	TinterStructType := TypeOf((*TinterSmallStruct)(nil)).Elem()

	// Helper function: check both direct and interface-converted calls for struct
	CheckStructI := func(name string, i any, inc int) {
		v := ValueOf(i)
		CheckStructV(name, v, inc)
		CheckStructV("(i="+name+")", v.Convert(TinterStructType), inc)
	}

	// Helper function: check return value of small array
	CheckArrayF := func(name string, f func(int, byte) ([1]byte, int), inc int) {
		ret, x := f(1000, 99)
		if ret[0] != byte(99) || x != 1000+inc {
			t.Errorf("%s(1000, 99) = [%v], %v, want [99], %v", name, ret[0], x, 1000+inc)
		}
	}

	// Helper function: check via reflection with small array return
	CheckArrayV := func(name string, i Value, inc int) {
		bx := i.Method(0).Call([]Value{ValueOf(1000), ValueOf(byte(99))})
		arrVal := bx[0]
		if arrVal.Len() < 1 {
			t.Errorf("returned array length insufficient")
			return
		}
		ret := arrVal.Index(0).Interface()
		x := bx[1].Interface()
		if ret != byte(99) || x != 1000+inc {
			t.Errorf("direct %s.M(1000, 99) = [%v], %v, want [99], %v", name, ret, x, 1000+inc)
		}
		CheckArrayF(name+".M", i.Method(0).Interface().(func(int, byte) ([1]byte, int)), inc)
	}

	TinterArrayType := TypeOf((*TinterSmallArray)(nil)).Elem()

	// Helper function: check both direct and interface-converted calls for array
	CheckArrayI := func(name string, i any, inc int) {
		v := ValueOf(i)
		CheckArrayV(name, v, inc)
		CheckArrayV("(i="+name+")", v.Convert(TinterArrayType), inc)
	}

	// Test cases for StructResult return type

	// Small receiver types (byte)
	ssv := TsrSmallv(1)
	CheckStructI("ssv", ssv, 1)
	CheckStructI("&ssv", &ssv, 1)

	ssp := TsrSmallp(2)
	CheckStructI("&ssp", &ssp, 2)

	// Word-sized receiver types (uintptr)
	wsv := TsrWordv(3)
	CheckStructI("wsv", wsv, 3)
	CheckStructI("&wsv", &wsv, 3)

	wsp := TsrWordp(4)
	CheckStructI("&wsp", &wsp, 4)

	// Large receiver types ([2]uintptr)
	bsv := TsrBigv([2]uintptr{5, 6})
	CheckStructI("bsv", bsv, 11)
	CheckStructI("&bsv", &bsv, 11)

	bsp := TsrBigp([2]uintptr{7, 8})
	CheckStructI("&bsp", &bsp, 15)

	// Embedded structs (pointer embedding chain)
	tsm4 := TsrEmb4{}
	tsm3 := TsrEmb3{&tsm4}
	tsm2 := TsrEmb2{&tsm3}
	tsm1 := TsrEmb1{tsm2}
	CheckStructI("tsm4", tsm4, 40)
	CheckStructI("&tsm4", &tsm4, 40)
	CheckStructI("tsm3", tsm3, 40)
	CheckStructI("&tsm3", &tsm3, 40)
	CheckStructI("tsm2", tsm2, 40)
	CheckStructI("&tsm2", &tsm2, 40)
	CheckStructI("tsm1", tsm1, 40)
	CheckStructI("&tsm1", &tsm1, 40)

	// Test cases for [1]byte return type

	// Small receiver types (byte)
	sav := TarSmallv(1)
	CheckArrayI("sav", sav, 1)
	CheckArrayI("&sav", &sav, 1)

	sap := TarSmallp(2)
	CheckArrayI("&sap", &sap, 2)

	// Word-sized receiver types (uintptr)
	wav := TarWordv(3)
	CheckArrayI("wav", wav, 3)
	CheckArrayI("&wav", &wav, 3)

	wap := TarWordp(4)
	CheckArrayI("&wap", &wap, 4)

	// Large receiver types ([2]uintptr)
	bav := TarBigv([2]uintptr{5, 6})
	CheckArrayI("bav", bav, 11)
	CheckArrayI("&bav", &bav, 11)

	bap := TarBigp([2]uintptr{7, 8})
	CheckArrayI("&bap", &bap, 15)

	// Embedded structs (pointer embedding chain)
	tam4 := TarEmb4{}
	tam3 := TarEmb3{&tam4}
	tam2 := TarEmb2{&tam3}
	tam1 := TarEmb1{tam2}
	CheckArrayI("tam4", tam4, 40)
	CheckArrayI("&tam4", &tam4, 40)
	CheckArrayI("tam3", tam3, 40)
	CheckArrayI("&tam3", &tam3, 40)
	CheckArrayI("tam2", tam2, 40)
	CheckArrayI("&tam2", &tam2, 40)
	CheckArrayI("tam1", tam1, 40)
	CheckArrayI("&tam1", &tam1, 40)
}

// Package-level type definitions for float32 return type
