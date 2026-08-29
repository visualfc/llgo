//go:build go1.27

package cl_test

import (
	"testing"

	"github.com/xgo-dev/llgo/cl/cltest"
)

func TestRunAndTestFromTest127(t *testing.T) {
	cltest.RunAndTestFromDir(t, "", "./_test127", nil)
}
