package main

import (
	"fmt"
	"os"

	"github.com/xgo-dev/llgo/internal/build/testdata/coverage/basic"
	"github.com/xgo-dev/llgo/internal/build/testdata/coverage/dep"
	"golang.org/x/mod/semver"
)

var initial = dep.Value()

func main() {
	defer fmt.Println("deferred")
	fmt.Println(initial, basic.Branch(len(os.Args) > 1), semver.Compare("v1.0.0", "v2.0.0"))
	if len(os.Args) > 1 {
		switch os.Args[1] {
		case "exit":
			os.Exit(7)
		case "panic":
			panic("coverage crash fixture")
		}
	}
}
