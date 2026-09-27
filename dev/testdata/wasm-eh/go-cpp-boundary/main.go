package main

import "github.com/xgo-dev/llgo/dev/testdata/wasm-eh/go-cpp-boundary/cpp"

func main() {
	status := cpp.Catch()
	if status != 7 {
		panic("C++ did not catch its exception")
	}
	defer func() {
		if recover() != status {
			panic("Go did not recover the translated status")
		}
		println("go cpp boundary ok")
	}()
	// Translate the C ABI status in Go. Never unwind a C++ exception through Go.
	panic(status)
}
