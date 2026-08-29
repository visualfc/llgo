//go:build !windows || !386

package main

import "github.com/goplus/lib/c"

func multiCallbackCalc() c.Pointer {
	return c.Func((*MultiBar).sqrt)
}

func multiCallbackVal() c.Pointer {
	return c.Func(multiIValGetA)
}
