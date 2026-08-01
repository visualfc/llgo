package main

import (
	"fmt"
	"os"
	"runtime"
)

func init() {
	done := make(chan int, 1)
	defer func() {
		done <- 0
	}()
	go func() {
		code := <-done
		fmt.Println("ok")
		os.Exit(code)
	}()
	runtime.Goexit()
}

func main() {}
