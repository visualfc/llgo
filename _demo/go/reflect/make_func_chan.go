package main

import . "reflect"

func TestFuncOf(t *testingT) {
	// check construction and use of type not in binary
	type K string
	type V float64

	fn := func(args []Value) []Value {
		if len(args) != 1 {
			t.Errorf("args == %v, want exactly one arg", args)
		} else if args[0].Type() != TypeOf(K("")) {
			t.Errorf("args[0] is type %v, want %v", args[0].Type(), TypeOf(K("")))
		} else if args[0].String() != "gopher" {
			t.Errorf("args[0] = %q, want %q", args[0].String(), "gopher")
		}
		return []Value{ValueOf(V(3.14))}
	}
	v := MakeFunc(FuncOf([]Type{TypeOf(K(""))}, []Type{TypeOf(V(0))}, false), fn)

	outs := v.Call([]Value{ValueOf(K("gopher"))})
	if len(outs) != 1 {
		t.Fatalf("v.Call returned %v, want exactly one result", outs)
	} else if outs[0].Type() != TypeOf(V(0)) {
		t.Fatalf("c.Call[0] is type %v, want %v", outs[0].Type(), TypeOf(V(0)))
	}
	f := outs[0].Float()
	if f != 3.14 {
		t.Errorf("constructed func returned %f, want %f", f, 3.14)
	}

	// check that types already in binary are found
	type T1 int
	testCases := []struct {
		in, out  []Type
		variadic bool
		want     any
	}{
		{in: []Type{TypeOf(T1(0))}, want: (func(T1))(nil)},
		{in: []Type{TypeOf(int(0))}, want: (func(int))(nil)},
		{in: []Type{SliceOf(TypeOf(int(0)))}, variadic: true, want: (func(...int))(nil)},
		{in: []Type{TypeOf(int(0))}, out: []Type{TypeOf(false)}, want: (func(int) bool)(nil)},
		{in: []Type{TypeOf(int(0))}, out: []Type{TypeOf(false), TypeOf("")}, want: (func(int) (bool, string))(nil)},
	}
	for _, tt := range testCases {
		checkSameType(t, FuncOf(tt.in, tt.out, tt.variadic), tt.want)
	}

	// check that variadic requires last element be a slice.
	FuncOf([]Type{TypeOf(1), TypeOf(""), SliceOf(TypeOf(false))}, nil, true)
	shouldPanic("must be slice", func() { FuncOf([]Type{TypeOf(0), TypeOf(""), TypeOf(false)}, nil, true) })
	shouldPanic("must be slice", func() { FuncOf(nil, nil, true) })

	//testcase for  #54669
	var in []Type
	for i := 0; i < 51; i++ {
		in = append(in, TypeOf(1))
	}
	FuncOf(in, nil, false)
}

func TestChanOfDir(t *testingT) {
	// check construction and use of type not in binary
	type T string
	crt := ChanOf(RecvDir, TypeOf(T("")))
	cst := ChanOf(SendDir, TypeOf(T("")))

	// check that type already in binary is found
	type T1 int
	checkSameType(t, ChanOf(RecvDir, TypeOf(T1(1))), (<-chan T1)(nil))
	checkSameType(t, ChanOf(SendDir, TypeOf(T1(1))), (chan<- T1)(nil))

	// check String form of ChanDir
	if crt.ChanDir().String() != "<-chan" {
		t.Errorf("chan dir: have %q, want %q", crt.ChanDir().String(), "<-chan")
	}
	if cst.ChanDir().String() != "chan<-" {
		t.Errorf("chan dir: have %q, want %q", cst.ChanDir().String(), "chan<-")
	}
}
