//go:build !windows

package main

import (
	"github.com/goplus/lib/c/math/rand"
	ctime "github.com/goplus/lib/c/time"
)

func main() {
	var realtime, monotonic ctime.Timespec
	if ctime.ClockGettime(ctime.CLOCK_REALTIME, &realtime) != 0 ||
		ctime.ClockGettime(ctime.CLOCK_MONOTONIC, &monotonic) != 0 {
		panic("clock_gettime")
	}
	if realtime.Sec <= 0 || monotonic.Sec < 0 {
		panic("invalid C clock")
	}

	rand.Srand(1)
	first := rand.Rand()
	rand.Srand(1)
	if rand.Rand() != first {
		panic("C rand seed")
	}
	rand.Srandom(1)
	random := rand.Random()
	rand.Srandom(1)
	if rand.Random() != random {
		panic("C random seed")
	}
}
