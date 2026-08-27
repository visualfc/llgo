//go:build windows

package main

import (
	"fmt"

	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/math/rand"
	"github.com/goplus/lib/c/time"
)

func main() {
	rand.Srand(c.Uint(time.Time(nil)))
	fmt.Printf("%x\n", rand.Rand())
	fmt.Printf("%x\n", rand.Rand())
}
