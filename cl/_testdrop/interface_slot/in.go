// LITTEST
package main

// SYMBOL-NOT: testdrop/interface_slot{{.*}}T{{.*}}Drop
// SYMBOL-DAG: testdrop/interface_slot{{.*}}T{{.*}}Keep
// SYMBOL-NOT: testdrop/interface_slot{{.*}}T{{.*}}Drop

type I interface {
	Keep() int
}

// This case converts T to a non-empty interface and calls I.Keep. T implements
// I, so T.Keep must remain reachable, while T.Drop is not required by the
// reachable interface method demand.
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

func use(i I) int {
	return i.Keep()
}

func main() {
	println(use(T{n: 41}))
}
