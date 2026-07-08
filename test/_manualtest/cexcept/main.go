package main

import (
	"fmt"
	"os"
	"runtime/debug"
	_ "unsafe"
)

const (
	LLGoFiles = "wrap/fault.c"
)

//go:linkname cexcSegv C.cexc_segv
func cexcSegv(depth int32)

//go:linkname cexcDiv C.cexc_div
func cexcDiv(den int32) int32

//go:noinline
func callC(kind string) {
	switch kind {
	case "segv":
		cexcSegv(2) // C: cexc_segv -> cexc_mid_segv x3 -> cexc_leaf_segv -> *NULL
	case "div":
		fmt.Println("div result:", cexcDiv(0)) // arm64 does not trap (returns 0); amd64 raises SIGFPE
	}
}

//go:noinline
func viaGo(kind string) {
	callC(kind)
}

func main() {
	kind, mode := "segv", "recover"
	if len(os.Args) > 1 {
		kind = os.Args[1]
	}
	if len(os.Args) > 2 {
		mode = os.Args[2]
	}
	if mode == "recover" {
		defer func() {
			if r := recover(); r != nil {
				fmt.Println("recovered:", r)
				os.Stdout.Write(debug.Stack())
			} else {
				fmt.Println("no panic for:", kind)
			}
		}()
	}
	viaGo(kind)
	fmt.Println("survived", kind)
}
