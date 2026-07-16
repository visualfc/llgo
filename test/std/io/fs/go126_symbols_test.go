//go:build go1.26

package fs_test

import (
	"io/fs"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var _ fs.ReadLinkFS
	_ = fs.ReadLink
}
