//go:build go1.26

package fstest_test

import (
	"testing"
	"testing/fstest"
)

func TestGo126Symbols(t *testing.T) {
	var filesystem fstest.MapFS
	_ = filesystem.Lstat
	_ = filesystem.ReadLink
}
