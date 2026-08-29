//go:build windows

package main

import (
	"github.com/goplus/lib/c/math/rand"
	ctime "github.com/goplus/lib/c/time"
)

func main() {
	if ctime.Time(nil) <= 0 || ctime.Clock() < 0 {
		panic("invalid C clock")
	}
	rand.Srand(1)
	first := rand.Rand()
	rand.Srand(1)
	if rand.Rand() != first {
		panic("C rand seed")
	}
	verifyGenerators()
}
