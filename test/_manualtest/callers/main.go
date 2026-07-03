package main

import (
	"fmt"
	"runtime"
	"sync"
)

type thing struct{}

var sinkA, sinkB int

//go:noinline
func (thing) valueMethod() uintptr { sinkA++; pc, _, _, _ := runtime.Caller(0); return pc }

//go:noinline
func (*thing) ptrMethod() uintptr { sinkB++; pc, _, _, _ := runtime.Caller(0); return pc }

//go:noinline
func level3() {
	// Caller(skip): 0=self 1=level2 2=level1 3=main
	for skip := 0; skip <= 3; skip++ {
		_, file, line, ok := runtime.Caller(skip)
		fmt.Printf("Caller(%d): %s:%d ok=%v\n", skip, short(file), line, ok)
	}
	var pcs [16]uintptr
	n := runtime.Callers(0, pcs[:])
	frames := runtime.CallersFrames(pcs[:n])
	for {
		f, more := frames.Next()
		fmt.Printf("frame: %-28s %s:%d\n", f.Function, short(f.File), f.Line)
		if !more {
			break
		}
	}
}

//go:noinline
func level2() { level3() }

//go:noinline
func level1() { level2() }

func short(p string) string {
	for i := len(p) - 1; i >= 0; i-- {
		if p[i] == '/' {
			return p[i+1:]
		}
	}
	return p
}

type stackErr struct {
	pcs [8]uintptr
	n   int
}

//go:noinline
func newStackErr() *stackErr {
	e := &stackErr{}
	e.n = runtime.Callers(1, e.pcs[:])
	return e
}

func main() {
	level1()

	var t thing
	fmt.Println("valueMethod:", runtime.FuncForPC(t.valueMethod()).Name())
	fmt.Println("ptrMethod:  ", runtime.FuncForPC((&t).ptrMethod()).Name())

	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		_, file, line, _ := runtime.Caller(0)
		fmt.Printf("goroutine Caller: %s:%d\n", short(file), line)
	}()
	wg.Wait()

	e := newStackErr() // capture site: deferred symbolization must report this line
	frames := runtime.CallersFrames(e.pcs[:e.n])
	for {
		f, more := frames.Next()
		if f.Function == "main.main" {
			fmt.Printf("stackErr captured at: %s:%d\n", short(f.File), f.Line)
		}
		if !more {
			break
		}
	}
}
