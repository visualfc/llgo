// LITTEST
package main

// SYMBOL-NOT: testdrop/direct_func{{.*}}Drop
// SYMBOL-DAG: testdrop/direct_func{{.*}}Keep
// SYMBOL-NOT: testdrop/direct_func{{.*}}Drop

var keepFunc = Keep

//go:noinline
func Keep() int {
	return 42
}

//go:noinline
func Drop() int {
	panic("Drop should be unreachable")
}

func main() {
	println(keepFunc())
}
