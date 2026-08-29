//go:build !windows

package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/pthread"
)

var key pthread.Key

func main() {
	if key.Create(nil) != 0 {
		panic("pthread_key_create")
	}
	defer key.Delete()
	if key.Set(c.Pointer(c.Str("main value"))) != 0 {
		panic("pthread_setspecific main")
	}

	var thd pthread.Thread
	if pthread.Create(&thd, nil, func(arg c.Pointer) c.Pointer {
		if key.Set(c.Pointer(c.Str("thread value"))) != 0 {
			return nil
		}
		if got := c.GoString((*c.Char)(key.Get())); got != "thread value" {
			return nil
		}
		return c.Pointer(c.Str("joined"))
	}, nil) != 0 {
		panic("pthread_create")
	}

	var retval c.Pointer
	if pthread.Join(thd, &retval) != 0 || retval == nil || c.GoString((*c.Char)(retval)) != "joined" {
		panic("pthread_join")
	}
	if got := c.GoString((*c.Char)(key.Get())); got != "main value" {
		panic("pthread TLS locality")
	}
}
