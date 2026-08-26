package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/pthread"
)

var key pthread.Key

func main() {
	if key.Create(nil) != 0 {
		panic("key create failed")
	}
	defer key.Delete()
	if key.Set(c.Pointer(c.Str("main value\n"))) != 0 {
		panic("main key set failed")
	}

	var thd pthread.Thread
	if err := pthread.Create(&thd, nil, func(arg c.Pointer) c.Pointer {
		if key.Set(c.Pointer(c.Str("thread value\n"))) != 0 {
			return c.Pointer(c.Str("thread key set failed"))
		}
		if c.GoString((*c.Char)(key.Get())) != "thread value\n" {
			return c.Pointer(c.Str("thread key read failed"))
		}
		c.Printf(c.Str("Hello, thread\nTLS: %s"), key.Get())
		return c.Pointer(c.Str("Back to main\n"))
	}, nil); err != 0 {
		panic("thread create failed")
	}

	var retval c.Pointer
	if pthread.Join(thd, &retval) != 0 {
		panic("thread join failed")
	}
	if c.GoString((*c.Char)(retval)) != "Back to main\n" {
		panic(c.GoString((*c.Char)(retval)))
	}
	if c.GoString((*c.Char)(key.Get())) != "main value\n" {
		panic("main TLS value changed")
	}

	c.Printf(c.Str("%sTLS: %s"), retval, key.Get())
	c.Fflush(c.Stdout)
}
