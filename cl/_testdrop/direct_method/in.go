// LITTEST
package main

// SYMBOL-NOT: testdrop/direct_method{{.*}}T{{.*}}Drop
// SYMBOL-DAG: testdrop/direct_method{{.*}}T{{.*}}Keep
// SYMBOL-NOT: testdrop/direct_method{{.*}}T{{.*}}Drop

var sink any
var keepMethod = T.Keep

// This case keeps T's runtime type metadata reachable through an empty
// interface conversion. T.Drop must still be removed because no interface or
// direct call path requires that method slot.
type T struct {
	n int
}

//go:noinline
func (t T) Keep() int {
	return t.n + 1
}

//go:noinline
func (t T) Drop() int {
	panic("Drop should be unreachable")
}

func main() {
	t := T{n: 41}
	sink = t
	println(keepMethod(t))
}
