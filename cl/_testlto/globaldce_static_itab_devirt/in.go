// LITTEST
package main

import "reflect"

// SYMBOL-DAG: main{{.*}}A{{.*}}M
// SYMBOL-NOT: main{{.*}}B{{.*}}M
// SYMBOL-NOT: _llgo_itab

type I interface {
	M() int
}

type A struct{}
type B struct{}

//go:noinline
func (A) M() int {
	return 7
}

//go:noinline
func (B) M() int {
	panic("B.M should be unreachable")
}

// Keep the interface value across a function boundary so the LTO plugin must
// prove that every caller supplies the same static itab.
//
//go:noinline
func callM(v I) int {
	return v.M()
}

//go:noinline
func keepType(v any) bool {
	return reflect.TypeOf(v).Name() == "B"
}

//go:noinline
func reflectedI(v A) I {
	return reflect.ValueOf(v).Interface().(I)
}

func main() {
	// Keep B's type descriptor live without creating a B-to-I conversion. A
	// signature-wide method capability would retain B.M through that descriptor.
	ok := keepType(B{})
	direct := I(A{})
	// Interface equality relies on canonical runtime itab identity. The static
	// template is analysis-only; the direct conversion must still agree with an
	// interface assembled through reflection.
	println(callM(direct), direct == reflectedI(A{}), ok)
}
