package main

import (
	"github.com/goplus/lib/c/sync/atomic"
)

func main() {
	var v int64

	atomic.Store(&v, 100)
	if atomic.Load(&v) != 100 {
		panic("atomic load/store")
	}

	ret := atomic.Add(&v, 1)
	if ret != 100 || v != 101 {
		panic("atomic add")
	}

	ret, exchanged := atomic.CompareAndExchange(&v, 100, 102)
	if ret != 101 || exchanged || v != 101 {
		panic("atomic compare exchange miss")
	}

	ret, exchanged = atomic.CompareAndExchange(&v, 101, 102)
	if ret != 101 || !exchanged || v != 102 {
		panic("atomic compare exchange hit")
	}

	ret = atomic.Sub(&v, 1)
	if ret != 102 || v != 101 {
		panic("atomic sub")
	}
}
