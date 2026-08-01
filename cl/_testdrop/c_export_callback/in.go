// LITTEST
package main

/*
extern int Callback(void);
static int callCallback(void) {
	return Callback();
}
*/
import "C"

// SYMBOL-NOT: main{{.*}}T{{.*}}Drop
// SYMBOL-DAG: main{{.*}}T{{.*}}M
// SYMBOL-NOT: main{{.*}}T{{.*}}Drop

type I interface {
	M() int
}

type T struct{}

//go:noinline
func (T) M() int { return 7 }

//go:noinline
func (T) Drop() int { panic("Drop should be unreachable") }

//export Callback
func Callback() C.int {
	var v I = T{}
	return C.int(v.M())
}

func main() {
	println(C.callCallback())
}
