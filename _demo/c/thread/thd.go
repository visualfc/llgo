package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/pthread"
)

//llgo:tls
var threadValue uintptr

func main() {
	threadValue = uintptr(c.Pointer(c.Str("main value\n")))

	var thd pthread.Thread
	pthread.Create(&thd, nil, func(arg c.Pointer) c.Pointer {
		threadValue = uintptr(c.Pointer(c.Str("thread value\n")))
		c.Printf(c.Str("Hello, thread\nTLS: %s"), c.Pointer(threadValue))
		return c.Pointer(c.Str("Back to main\n"))
	}, nil)

	var retval c.Pointer
	pthread.Join(thd, &retval)

	c.Printf(c.Str("%sTLS: %s"), retval, c.Pointer(threadValue))
}
