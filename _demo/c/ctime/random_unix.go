//go:build !windows

package main

import "github.com/goplus/lib/c/math/rand"

func fastRand64() uint64 {
	low := uint64(rand.Random())
	high := uint64(rand.Random())
	return low ^ high<<32
}
