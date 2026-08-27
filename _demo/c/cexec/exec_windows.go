package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/os"
)

func main() {
	cmd := c.Str("cmd.exe")
	os.Execlp(cmd, cmd, c.Str("/c"), c.Str("echo Hello from execlp"), nil)
	panic("_execlp returned")
}
