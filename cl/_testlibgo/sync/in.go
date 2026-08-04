// LITTEST
package main

// CHECK-DAG: call {{.*}}sync{{.*}}Once{{.*}}Do{{.*}}
// CHECK-DAG: define void @"main.f$1"(ptr {{((nest|swiftself) )?}}%{{.*}}){{.*}} {

import (
	"sync"
)

var once sync.Once

func f(s string) {
	once.Do(func() {
		println(s)
	})
}

func main() {
	f("Do once")
	f("Do twice")
}
