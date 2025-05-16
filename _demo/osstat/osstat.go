package main

import (
	"fmt"
	"os"
)

func main() {
	s, err := os.Stat("main.go")
	if err != nil {
		panic(err)
	}
	if s.Name() != "main.go" {
		panic("bad name")
	}
	fmt.Printf("File:\t%v\n", s.Name())
	fmt.Printf("Size:\t%v\n", s.Size())
	fmt.Printf("Access:\t%o\n", s.Mode())
	fmt.Printf("Modified:\t%v\n", s.ModTime())
}
