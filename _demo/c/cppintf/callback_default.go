//go:build !windows || !386

package main

import "github.com/goplus/lib/c"

func callbackVal() c.Pointer {
	return c.Func((*Bar).getA)
}

func callbackCalc() c.Pointer {
	return c.Func((*Bar).sqrt)
}
