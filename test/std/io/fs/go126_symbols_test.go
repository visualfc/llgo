//go:build go1.26

package fs_test

import (
	"io/fs"
	"testing"
	"testing/fstest"
)

func TestReadLinkFS(t *testing.T) {
	filesystem := fstest.MapFS{
		"target.txt": &fstest.MapFile{Data: []byte("target")},
		"link.txt":   &fstest.MapFile{Data: []byte("target.txt"), Mode: fs.ModeSymlink},
	}
	var readLinkFS fs.ReadLinkFS = filesystem
	got, err := fs.ReadLink(readLinkFS, "link.txt")
	if err != nil || got != "target.txt" {
		t.Fatalf("ReadLink = %q, %v; want %q, nil", got, err, "target.txt")
	}
	info, err := readLinkFS.Lstat("link.txt")
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode()&fs.ModeSymlink == 0 {
		t.Fatalf("Lstat mode = %v, want symlink", info.Mode())
	}
}
