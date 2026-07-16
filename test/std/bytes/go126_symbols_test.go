//go:build go1.26

package bytes_test

import (
	"bytes"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var buffer bytes.Buffer
	_ = buffer.Peek
}
