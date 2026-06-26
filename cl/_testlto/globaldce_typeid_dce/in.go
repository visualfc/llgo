// LITTEST
package main

// CHECK-DAG: call { ptr, i1 } @llvm.type.checked.load(ptr %{{[0-9]+}}, i32 0, metadata !"go.method.Keep:func() int")
// CHECK-DAG: !"go.method.Keep:func() int"
// CHECK-DAG: !"go.method.Drop:func() int"
// CHECK-DAG: !"go.method.DropWithArg:func(int) int"
// CHECK-DAG: !"go.method.value.reflect"
// CHECK-DAG: !"go.method.type.reflect"

type Keeper interface {
	Keep() int
}

type Worker struct {
	n int
}

func (w Worker) Keep() int {
	return w.n + 10
}

func (w Worker) Drop() int {
	panic("Drop should be unreachable")
}

func (w Worker) DropWithArg(v int) int {
	panic("DropWithArg should be unreachable")
}

func call(k Keeper) int {
	return k.Keep()
}

func main() {
	println(call(Worker{n: 7}))
}
