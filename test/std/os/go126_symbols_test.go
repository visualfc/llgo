//go:build go1.26

package os_test

import (
	"os"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var process os.Process
	var root os.Root
	_ = os.ErrNoHandle
	_ = process.WithHandle
	_ = root.Chmod
	_ = root.Chown
	_ = root.Chtimes
	_ = root.Lchown
	_ = root.Link
	_ = root.MkdirAll
	_ = root.ReadFile
	_ = root.Readlink
	_ = root.RemoveAll
	_ = root.Rename
	_ = root.Symlink
	_ = root.WriteFile
}
