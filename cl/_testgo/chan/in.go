// LITTEST
package main

// CHECK-DAG: call ptr @"{{.*}}/runtime/internal/runtime.NewChan"({{.*}})
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.NewProc"({{.*}})
// CHECK-DAG: call i1 @"{{.*}}/runtime/internal/runtime.ChanRecv"({{.*}})
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.ChanClose"({{.*}})

func main() {
	ch := make(chan int, 10)
	var v any = ch
	println(ch, len(ch), cap(ch), v)
	go func() {
		ch <- 100
	}()
	n := <-ch
	println(n)

	ch2 := make(chan int, 10)
	go func() {
		close(ch2)
	}()
	n2, ok := <-ch2
	println(n2, ok)
}
