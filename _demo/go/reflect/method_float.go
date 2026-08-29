package main

import . "reflect"

type TinterFloat32 interface {
	M(int, byte) (float32, int)
}

type T32Smallv byte

func (v T32Smallv) M(x int, b byte) (float32, int) {
	return float32(b), x + int(v)
}

type T32Smallp byte

func (p *T32Smallp) M(x int, b byte) (float32, int) {
	return float32(b), x + int(*p)
}

type T32Wordv uintptr

func (v T32Wordv) M(x int, b byte) (float32, int) {
	return float32(b), x + int(v)
}

type T32Wordp uintptr

func (p *T32Wordp) M(x int, b byte) (float32, int) {
	return float32(b), x + int(*p)
}

type T32Bigv [2]uintptr

func (v T32Bigv) M(x int, b byte) (float32, int) {
	return float32(b), x + int(v[0]) + int(v[1])
}

type T32Bigp [2]uintptr

func (p *T32Bigp) M(x int, b byte) (float32, int) {
	return float32(b), x + int(p[0]) + int(p[1])
}

// Embedding via pointer for float32 return type
type T32Emb1 struct{ T32Emb2 }
type T32Emb2 struct{ *T32Emb3 }
type T32Emb3 struct{ *T32Emb4 }
type T32Emb4 struct{}

func (t4 T32Emb4) M(x int, b byte) (float32, int) {
	return float32(b), x + 40
}

// Package-level type definitions for float64 return type
type TinterFloat64 interface {
	M(int, byte) (float64, int)
}

type T64Smallv byte

func (v T64Smallv) M(x int, b byte) (float64, int) {
	return float64(b), x + int(v)
}

type T64Smallp byte

func (p *T64Smallp) M(x int, b byte) (float64, int) {
	return float64(b), x + int(*p)
}

type T64Wordv uintptr

func (v T64Wordv) M(x int, b byte) (float64, int) {
	return float64(b), x + int(v)
}

type T64Wordp uintptr

func (p *T64Wordp) M(x int, b byte) (float64, int) {
	return float64(b), x + int(*p)
}

type T64Bigv [2]uintptr

func (v T64Bigv) M(x int, b byte) (float64, int) {
	return float64(b), x + int(v[0]) + int(v[1])
}

type T64Bigp [2]uintptr

func (p *T64Bigp) M(x int, b byte) (float64, int) {
	return float64(b), x + int(p[0]) + int(p[1])
}

// Embedding via pointer for float64 return type
type T64Emb1 struct{ T64Emb2 }
type T64Emb2 struct{ *T64Emb3 }
type T64Emb3 struct{ *T64Emb4 }
type T64Emb4 struct{}

func (t4 T64Emb4) M(x int, b byte) (float64, int) {
	return float64(b), x + 40
}

// Package-level type definitions for float32 struct return type
type Float32Struct struct{ N float32 }

type TinterFloat32Struct interface {
	M(int, byte) (Float32Struct, int)
}

type T32sSmallv byte

func (v T32sSmallv) M(x int, b byte) (Float32Struct, int) {
	return Float32Struct{float32(b)}, x + int(v)
}

type T32sSmallp byte

func (p *T32sSmallp) M(x int, b byte) (Float32Struct, int) {
	return Float32Struct{float32(b)}, x + int(*p)
}

type T32sWordv uintptr

func (v T32sWordv) M(x int, b byte) (Float32Struct, int) {
	return Float32Struct{float32(b)}, x + int(v)
}

type T32sWordp uintptr

func (p *T32sWordp) M(x int, b byte) (Float32Struct, int) {
	return Float32Struct{float32(b)}, x + int(*p)
}

type T32sBigv [2]uintptr

func (v T32sBigv) M(x int, b byte) (Float32Struct, int) {
	return Float32Struct{float32(b)}, x + int(v[0]) + int(v[1])
}

type T32sBigp [2]uintptr

func (p *T32sBigp) M(x int, b byte) (Float32Struct, int) {
	return Float32Struct{float32(b)}, x + int(p[0]) + int(p[1])
}

// Embedding via pointer for float32 struct return type
type T32sEmb1 struct{ T32sEmb2 }
type T32sEmb2 struct{ *T32sEmb3 }
type T32sEmb3 struct{ *T32sEmb4 }
type T32sEmb4 struct{}

func (t4 T32sEmb4) M(x int, b byte) (Float32Struct, int) {
	return Float32Struct{float32(b)}, x + 40
}

// TestMethodFloat tests methods returning float32, float64, and float32 struct
func TestMethodFloat(t *testingT) {
	// Helper function: check return value of float32
	CheckFloat32F := func(name string, f func(int, byte) (float32, int), inc int) {
		ret, x := f(1000, 99)
		if ret != float32(99) || x != 1000+inc {
			t.Errorf("%s(1000, 99) = %v, %v, want 99, %v", name, ret, x, 1000+inc)
		}
	}

	CheckFloat32V := func(name string, i Value, inc int) {
		bx := i.Method(0).Call([]Value{ValueOf(1000), ValueOf(byte(99))})
		ret := bx[0].Interface()
		x := bx[1].Interface()
		if ret != float32(99) || x != 1000+inc {
			t.Errorf("direct %s.M(1000, 99) = %v, %v, want 99, %v", name, ret, x, 1000+inc)
		}
		CheckFloat32F(name+".M", i.Method(0).Interface().(func(int, byte) (float32, int)), inc)
	}

	TinterFloat32Type := TypeOf((*TinterFloat32)(nil)).Elem()

	CheckFloat32I := func(name string, i any, inc int) {
		v := ValueOf(i)
		CheckFloat32V(name, v, inc)
		CheckFloat32V("(i="+name+")", v.Convert(TinterFloat32Type), inc)
	}

	// Helper function: check return value of float64
	CheckFloat64F := func(name string, f func(int, byte) (float64, int), inc int) {
		ret, x := f(1000, 99)
		if ret != float64(99) || x != 1000+inc {
			t.Errorf("%s(1000, 99) = %v, %v, want 99, %v", name, ret, x, 1000+inc)
		}
	}

	CheckFloat64V := func(name string, i Value, inc int) {
		bx := i.Method(0).Call([]Value{ValueOf(1000), ValueOf(byte(99))})
		ret := bx[0].Interface()
		x := bx[1].Interface()
		if ret != float64(99) || x != 1000+inc {
			t.Errorf("direct %s.M(1000, 99) = %v, %v, want 99, %v", name, ret, x, 1000+inc)
		}
		CheckFloat64F(name+".M", i.Method(0).Interface().(func(int, byte) (float64, int)), inc)
	}

	TinterFloat64Type := TypeOf((*TinterFloat64)(nil)).Elem()

	CheckFloat64I := func(name string, i any, inc int) {
		v := ValueOf(i)
		CheckFloat64V(name, v, inc)
		CheckFloat64V("(i="+name+")", v.Convert(TinterFloat64Type), inc)
	}

	// Helper function: check return value of float32 struct
	CheckFloat32StructF := func(name string, f func(int, byte) (Float32Struct, int), inc int) {
		ret, x := f(1000, 99)
		if ret.N != float32(99) || x != 1000+inc {
			t.Errorf("%s(1000, 99) = {%v}, %v, want {99}, %v", name, ret.N, x, 1000+inc)
		}
	}

	CheckFloat32StructV := func(name string, i Value, inc int) {
		bx := i.Method(0).Call([]Value{ValueOf(1000), ValueOf(byte(99))})
		structVal := bx[0]
		ret := structVal.Field(0).Interface()
		x := bx[1].Interface()
		if ret != float32(99) || x != 1000+inc {
			t.Errorf("direct %s.M(1000, 99) = {%v}, %v, want {99}, %v", name, ret, x, 1000+inc)
		}
		CheckFloat32StructF(name+".M", i.Method(0).Interface().(func(int, byte) (Float32Struct, int)), inc)
	}

	TinterFloat32StructType := TypeOf((*TinterFloat32Struct)(nil)).Elem()

	CheckFloat32StructI := func(name string, i any, inc int) {
		v := ValueOf(i)
		CheckFloat32StructV(name, v, inc)
		CheckFloat32StructV("(i="+name+")", v.Convert(TinterFloat32StructType), inc)
	}

	// Test cases for float32 return type

	// Small receiver types (byte)
	s32v := T32Smallv(1)
	CheckFloat32I("s32v", s32v, 1)
	CheckFloat32I("&s32v", &s32v, 1)

	s32p := T32Smallp(2)
	CheckFloat32I("&s32p", &s32p, 2)

	// Word-sized receiver types (uintptr)
	w32v := T32Wordv(3)
	CheckFloat32I("w32v", w32v, 3)
	CheckFloat32I("&w32v", &w32v, 3)

	w32p := T32Wordp(4)
	CheckFloat32I("&w32p", &w32p, 4)

	// Large receiver types ([2]uintptr)
	b32v := T32Bigv([2]uintptr{5, 6})
	CheckFloat32I("b32v", b32v, 11)
	CheckFloat32I("&b32v", &b32v, 11)

	b32p := T32Bigp([2]uintptr{7, 8})
	CheckFloat32I("&b32p", &b32p, 15)

	// Embedded structs (pointer embedding chain) for float32
	t32m4 := T32Emb4{}
	t32m3 := T32Emb3{&t32m4}
	t32m2 := T32Emb2{&t32m3}
	t32m1 := T32Emb1{t32m2}
	CheckFloat32I("t32m4", t32m4, 40)
	CheckFloat32I("&t32m4", &t32m4, 40)
	CheckFloat32I("t32m3", t32m3, 40)
	CheckFloat32I("&t32m3", &t32m3, 40)
	CheckFloat32I("t32m2", t32m2, 40)
	CheckFloat32I("&t32m2", &t32m2, 40)
	CheckFloat32I("t32m1", t32m1, 40)
	CheckFloat32I("&t32m1", &t32m1, 40)

	// Test cases for float64 return type

	// Small receiver types (byte)
	s64v := T64Smallv(1)
	CheckFloat64I("s64v", s64v, 1)
	CheckFloat64I("&s64v", &s64v, 1)

	s64p := T64Smallp(2)
	CheckFloat64I("&s64p", &s64p, 2)

	// Word-sized receiver types (uintptr)
	w64v := T64Wordv(3)
	CheckFloat64I("w64v", w64v, 3)
	CheckFloat64I("&w64v", &w64v, 3)

	w64p := T64Wordp(4)
	CheckFloat64I("&w64p", &w64p, 4)

	// Large receiver types ([2]uintptr)
	b64v := T64Bigv([2]uintptr{5, 6})
	CheckFloat64I("b64v", b64v, 11)
	CheckFloat64I("&b64v", &b64v, 11)

	b64p := T64Bigp([2]uintptr{7, 8})
	CheckFloat64I("&b64p", &b64p, 15)

	// Embedded structs (pointer embedding chain) for float64
	t64m4 := T64Emb4{}
	t64m3 := T64Emb3{&t64m4}
	t64m2 := T64Emb2{&t64m3}
	t64m1 := T64Emb1{t64m2}
	CheckFloat64I("t64m4", t64m4, 40)
	CheckFloat64I("&t64m4", &t64m4, 40)
	CheckFloat64I("t64m3", t64m3, 40)
	CheckFloat64I("&t64m3", &t64m3, 40)
	CheckFloat64I("t64m2", t64m2, 40)
	CheckFloat64I("&t64m2", &t64m2, 40)
	CheckFloat64I("t64m1", t64m1, 40)
	CheckFloat64I("&t64m1", &t64m1, 40)

	// Test cases for float32 struct return type

	// Small receiver types (byte)
	s32sv := T32sSmallv(1)
	CheckFloat32StructI("s32sv", s32sv, 1)
	CheckFloat32StructI("&s32sv", &s32sv, 1)

	s32sp := T32sSmallp(2)
	CheckFloat32StructI("&s32sp", &s32sp, 2)

	// Word-sized receiver types (uintptr)
	w32sv := T32sWordv(3)
	CheckFloat32StructI("w32sv", w32sv, 3)
	CheckFloat32StructI("&w32sv", &w32sv, 3)

	w32sp := T32sWordp(4)
	CheckFloat32StructI("&w32sp", &w32sp, 4)

	// Large receiver types ([2]uintptr)
	b32sv := T32sBigv([2]uintptr{5, 6})
	CheckFloat32StructI("b32sv", b32sv, 11)
	CheckFloat32StructI("&b32sv", &b32sv, 11)

	b32sp := T32sBigp([2]uintptr{7, 8})
	CheckFloat32StructI("&b32sp", &b32sp, 15)

	// Embedded structs (pointer embedding chain) for float32 struct
	t32sm4 := T32sEmb4{}
	t32sm3 := T32sEmb3{&t32sm4}
	t32sm2 := T32sEmb2{&t32sm3}
	t32sm1 := T32sEmb1{t32sm2}
	CheckFloat32StructI("t32sm4", t32sm4, 40)
	CheckFloat32StructI("&t32sm4", &t32sm4, 40)
	CheckFloat32StructI("t32sm3", t32sm3, 40)
	CheckFloat32StructI("&t32sm3", &t32sm3, 40)
	CheckFloat32StructI("t32sm2", t32sm2, 40)
	CheckFloat32StructI("&t32sm2", &t32sm2, 40)
	CheckFloat32StructI("t32sm1", t32sm1, 40)
	CheckFloat32StructI("&t32sm1", &t32sm1, 40)
}
