// LITTEST
package main

import "github.com/goplus/llgo/cl/_testdrop/exported_method_crosspkg/api"

// SYMBOL-NOT: testdrop/exported_method_crosspkg{{.*}}T{{.*}}Drop
// SYMBOL-DAG: testdrop/exported_method_crosspkg{{.*}}T{{.*}}Keep
// SYMBOL-NOT: testdrop/exported_method_crosspkg{{.*}}T{{.*}}Drop

// The interface type and dynamic call live in package api, while T and its
// method table live in this package. Exported method identity must let the
// global analysis match api.Keeper.Keep to T.Keep across package boundaries.
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
	println(api.Use(T{n: 41}))
}
