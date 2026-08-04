// LITTEST
package main

// CHECK-DAG: call i32 @pthread_once({{.*}})
// CHECK-DAG: define void @"main.f$1"(){{.*}} {

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/pthread/sync"
)

var once sync.Once = sync.OnceInit

func f() {
	once.Do(func() {
		c.Printf(c.Str("Do once\n"))
	})
}

func main() {
	println(c.GoString(c.Str("sync.Once demo\n"), 9))
	println(c.GoString(c.Str("sync.Once demo\n")))
	f()
	f()
}
