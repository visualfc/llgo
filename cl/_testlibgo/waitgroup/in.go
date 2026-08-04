// LITTEST
package main

// CHECK-DAG: call {{.*}}sync{{.*}}WaitGroup{{.*}}Add{{.*}}
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.NewProc"({{.*}})
// CHECK-DAG: call {{.*}}sync{{.*}}WaitGroup{{.*}}Done{{.*}}
// CHECK-DAG: call {{.*}}sync{{.*}}WaitGroup{{.*}}Wait{{.*}}

import (
	"sync"
)

func main() {
	var wg sync.WaitGroup
	wg.Add(2)
	go func() {
		defer wg.Done()
		println("work 1")
	}()
	go func() {
		defer wg.Done()
		println("work 2")
	}()
	wg.Wait()
	println("done")
}
