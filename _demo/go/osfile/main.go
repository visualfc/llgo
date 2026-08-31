package main

import (
	"errors"
	"io"
	"os"
	"path/filepath"
)

// This case owns the ordinary filesystem API surface. Keeping the operations
// in one temporary tree avoids compiling five tiny command packages while
// retaining the original Create/Open/Read/Write/Seek/Stat/Mkdir/ReadDir calls.
func main() {
	root, err := os.MkdirTemp("", "llgo-fileio-*")
	check(err)
	defer os.RemoveAll(root)
	scratch, err := os.CreateTemp(root, "scratch-*.txt")
	check(err)
	check(scratch.Close())

	nested := filepath.Join(root, "nested", "directory")
	check(os.MkdirAll(nested, 0o755))
	path := filepath.Join(nested, "data.txt")

	f, err := os.Create(path)
	check(err)
	if n, err := f.Write([]byte("Hello, World!\n")); err != nil || n != 14 {
		panic("Write")
	}
	if n, err := f.WriteString("Test WriteString\n"); err != nil || n != 17 {
		panic("WriteString")
	}
	check(f.Close())

	f, err = os.OpenFile(path, os.O_RDWR, 0o644)
	check(err)
	if n, err := f.WriteAt([]byte("XXXXX"), 0); err != nil || n != 5 {
		panic("WriteAt")
	}
	buf := make([]byte, 5)
	if n, err := f.ReadAt(buf, 7); err != nil || n != 5 || string(buf) != "World" {
		panic("ReadAt")
	}
	if off, err := f.Seek(7, io.SeekStart); err != nil || off != 7 {
		panic("Seek")
	}
	if n, err := f.Read(buf); err != nil || n != 5 || string(buf) != "World" {
		panic("Read after SeekStart")
	}
	if off, err := f.Seek(2, io.SeekCurrent); err != nil || off != 14 {
		panic("SeekCurrent")
	}
	if _, err := f.Seek(-5, io.SeekEnd); err != nil {
		panic("SeekEnd")
	}
	if n, err := f.WriteAt([]byte("YYYYY"), 7); err != nil || n != 5 {
		panic("WriteAt non-zero offset")
	}
	if n, err := f.ReadAt(buf, 7); err != nil || n != 5 || string(buf) != "YYYYY" {
		panic("ReadAt after WriteAt")
	}
	check(f.Truncate(19))
	info, err := f.Stat()
	check(err)
	if info.Size() != 19 {
		panic("Stat/Truncate")
	}
	check(f.Close())

	data, err := os.ReadFile(path)
	check(err)
	if string(data[:5]) != "XXXXX" {
		panic("ReadFile")
	}
	entries, err := os.ReadDir(nested)
	check(err)
	if len(entries) != 1 || entries[0].Name() != "data.txt" || entries[0].IsDir() {
		panic("ReadDir")
	}

	missing := filepath.Join(root, "missing")
	if _, err := os.Stat(missing); !errors.Is(err, os.ErrNotExist) || !os.IsNotExist(err) {
		panic("Stat missing")
	}
	if _, err := os.Open(missing); !errors.Is(err, os.ErrNotExist) {
		panic("Open missing")
	}
	println("fileio ok")
}

func check(err error) {
	if err != nil {
		panic(err)
	}
}
