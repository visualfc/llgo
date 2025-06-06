// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package os

import (
	"errors"
	"io"
	"syscall"
	"time"
)

// Name returns the name of the file as presented to Open.
func (f *File) Name() string { return f.name }

// Stdin, Stdout, and Stderr are open Files pointing to the standard input,
// standard output, and standard error file descriptors.
//
// Note that the Go runtime writes to standard error for panics and crashes;
// closing Stderr may cause those messages to go elsewhere, perhaps
// to a file opened later.
var (
	Stdin  = NewFile(uintptr(syscall.Stdin), "/dev/stdin")
	Stdout = NewFile(uintptr(syscall.Stdout), "/dev/stdout")
	Stderr = NewFile(uintptr(syscall.Stderr), "/dev/stderr")
)

// Flags to OpenFile wrapping those of the underlying system. Not all
// flags may be implemented on a given system.
const (
	// Exactly one of O_RDONLY, O_WRONLY, or O_RDWR must be specified.
	O_RDONLY int = syscall.O_RDONLY // open the file read-only.
	O_WRONLY int = syscall.O_WRONLY // open the file write-only.
	O_RDWR   int = syscall.O_RDWR   // open the file read-write.
	// The remaining values may be or'ed in to control behavior.
	O_APPEND int = syscall.O_APPEND // append data to the file when writing.
	O_CREATE int = syscall.O_CREAT  // create a new file if none exists.
	O_EXCL   int = syscall.O_EXCL   // used with O_CREATE, file must not exist.
	O_SYNC   int = syscall.O_SYNC   // open for synchronous I/O.
	O_TRUNC  int = syscall.O_TRUNC  // truncate regular writable file when opened.
)

// Seek whence values.
//
// Deprecated: Use io.SeekStart, io.SeekCurrent, and io.SeekEnd.
const (
	SEEK_SET int = 0 // seek relative to the origin of the file
	SEEK_CUR int = 1 // seek relative to the current offset
	SEEK_END int = 2 // seek relative to the end
)

// Read reads up to len(b) bytes from the File and stores them in b.
// It returns the number of bytes read and any error encountered.
// At end of file, Read returns 0, io.EOF.
func (f *File) Read(b []byte) (n int, err error) {
	if err := f.checkValid("read"); err != nil {
		return 0, err
	}
	n, e := f.read(b)
	return n, f.wrapErr("read", e)
}

// ReadAt reads len(b) bytes from the File starting at byte offset off.
// It returns the number of bytes read and the error, if any.
// ReadAt always returns a non-nil error when n < len(b).
// At end of file, that error is io.EOF.
func (f *File) ReadAt(b []byte, off int64) (n int, err error) {
	/*
		if err := f.checkValid("read"); err != nil {
			return 0, err
		}

		if off < 0 {
			return 0, &PathError{Op: "readat", Path: f.name, Err: errors.New("negative offset")}
		}

		for len(b) > 0 {
			m, e := f.pread(b, off)
			if e != nil {
				err = f.wrapErr("read", e)
				break
			}
			n += m
			b = b[m:]
			off += int64(m)
		}
		return
	*/
	panic("todo: os.File.ReadAt")
}

// ReadFrom implements io.ReaderFrom.
func (f *File) ReadFrom(r io.Reader) (n int64, err error) {
	/*
		if err := f.checkValid("write"); err != nil {
			return 0, err
		}
		n, handled, e := f.readFrom(r)
		if !handled {
			return genericReadFrom(f, r) // without wrapping
		}
		return n, f.wrapErr("write", e)
	*/
	panic("todo: os.File.ReadFrom")
}

func genericReadFrom(f *File, r io.Reader) (int64, error) {
	return io.Copy(fileWithoutReadFrom{f}, r)
}

// fileWithoutReadFrom implements all the methods of *File other
// than ReadFrom. This is used to permit ReadFrom to call io.Copy
// without leading to a recursive call to ReadFrom.
type fileWithoutReadFrom struct {
	*File
}

// This ReadFrom method hides the *File ReadFrom method.
func (fileWithoutReadFrom) ReadFrom(fileWithoutReadFrom) {
	panic("unreachable")
}

// Write writes len(b) bytes from b to the File.
// It returns the number of bytes written and an error, if any.
// Write returns a non-nil error when n != len(b).
func (f *File) Write(b []byte) (n int, err error) {
	if err := f.checkValid("write"); err != nil {
		return 0, err
	}
	n, e := f.write(b)

	// TODO(xsw):
	// epipecheck(f, e)

	if e != nil {
		err = f.wrapErr("write", e)
	} else if n != len(b) {
		err = io.ErrShortWrite
	}
	return n, err
}

var errWriteAtInAppendMode = errors.New("os: invalid use of WriteAt on file opened with O_APPEND")

// WriteAt writes len(b) bytes to the File starting at byte offset off.
// It returns the number of bytes written and an error, if any.
// WriteAt returns a non-nil error when n != len(b).
//
// If file was opened with the O_APPEND flag, WriteAt returns an error.
func (f *File) WriteAt(b []byte, off int64) (n int, err error) {
	/*
		if err := f.checkValid("write"); err != nil {
			return 0, err
		}
		if f.appendMode {
			return 0, errWriteAtInAppendMode
		}

		if off < 0 {
			return 0, &PathError{Op: "writeat", Path: f.name, Err: errors.New("negative offset")}
		}

		for len(b) > 0 {
			m, e := f.pwrite(b, off)
			if e != nil {
				err = f.wrapErr("write", e)
				break
			}
			n += m
			b = b[m:]
			off += int64(m)
		}
		return
	*/
	panic("todo: os.(*File).WriteAt")
}

// Seek sets the offset for the next Read or Write on file to offset, interpreted
// according to whence: 0 means relative to the origin of the file, 1 means
// relative to the current offset, and 2 means relative to the end.
// It returns the new offset and an error, if any.
// The behavior of Seek on a file opened with O_APPEND is not specified.
func (f *File) Seek(offset int64, whence int) (ret int64, err error) {
	/*
		if err := f.checkValid("seek"); err != nil {
			return 0, err
		}
		r, e := f.seek(offset, whence)
		if e == nil && f.dirinfo != nil && r != 0 {
			e = syscall.EISDIR
		}
		if e != nil {
			return 0, f.wrapErr("seek", e)
		}
		return r, nil
	*/
	panic("todo: os.(*File).Seek")
}

// WriteString is like Write, but writes the contents of string s rather than
// a slice of bytes.
func (f *File) WriteString(s string) (n int, err error) {
	/*
		b := unsafe.Slice(unsafe.StringData(s), len(s))
		return f.Write(b)
	*/
	panic("todo: os.(*File).WriteString")
}

// Open opens the named file for reading. If successful, methods on
// the returned file can be used for reading; the associated file
// descriptor has mode O_RDONLY.
// If there is an error, it will be of type *PathError.
func Open(name string) (*File, error) {
	return OpenFile(name, O_RDONLY, 0)
}

// Create creates or truncates the named file. If the file already exists,
// it is truncated. If the file does not exist, it is created with mode 0666
// (before umask). If successful, methods on the returned File can
// be used for I/O; the associated file descriptor has mode O_RDWR.
// If there is an error, it will be of type *PathError.
func Create(name string) (*File, error) {
	return OpenFile(name, O_RDWR|O_CREATE|O_TRUNC, 0666)
}

// OpenFile is the generalized open call; most users will use Open
// or Create instead. It opens the named file with specified flag
// (O_RDONLY etc.). If the file does not exist, and the O_CREATE flag
// is passed, it is created with mode perm (before umask). If successful,
// methods on the returned File can be used for I/O.
// If there is an error, it will be of type *PathError.
func OpenFile(name string, flag int, perm FileMode) (*File, error) {
	f, err := openFileNolog(name, flag, perm)
	if err != nil {
		return nil, err
	}
	f.appendMode = flag&O_APPEND != 0

	return f, nil
}

/*
// lstat is overridden in tests.
var lstat = Lstat

// Many functions in package syscall return a count of -1 instead of 0.
// Using fixCount(call()) instead of call() corrects the count.
func fixCount(n int, err error) (int, error) {
	if n < 0 {
		n = 0
	}
	return n, err
}
*/

// TODO(xsw):
// checkWrapErr is the test hook to enable checking unexpected wrapped errors of poll.ErrFileClosing.
// It is set to true in the export_test.go for tests (including fuzz tests).
// var checkWrapErr = false

// wrapErr wraps an error that occurred during an operation on an open file.
// It passes io.EOF through unchanged, otherwise converts
// poll.ErrFileClosing to ErrClosed and wraps the error in a PathError.
func (f *File) wrapErr(op string, err error) error {
	if err == nil || err == io.EOF {
		return err
	}
	/* TODO(xsw):
	if err == poll.ErrFileClosing {
		err = ErrClosed
	} else if checkWrapErr && errors.Is(err, poll.ErrFileClosing) {
		panic("unexpected error wrapping poll.ErrFileClosing: " + err.Error())
	}
	*/
	return &PathError{Op: op, Path: f.name, Err: err}
}

// TempDir returns the default directory to use for temporary files.
//
// On Unix systems, it returns $TMPDIR if non-empty, else /tmp.
// On Windows, it uses GetTempPath, returning the first non-empty
// value from %TMP%, %TEMP%, %USERPROFILE%, or the Windows directory.
// On Plan 9, it returns /tmp.
//
// The directory is neither guaranteed to exist nor have accessible
// permissions.
func TempDir() string {
	return tempDir()
}

// Chmod changes the mode of the file to mode.
// If there is an error, it will be of type *PathError.
func (f *File) Chmod(mode FileMode) error { return f.chmod(mode) }

// SetDeadline sets the read and write deadlines for a File.
// It is equivalent to calling both SetReadDeadline and SetWriteDeadline.
//
// Only some kinds of files support setting a deadline. Calls to SetDeadline
// for files that do not support deadlines will return ErrNoDeadline.
// On most systems ordinary files do not support deadlines, but pipes do.
//
// A deadline is an absolute time after which I/O operations fail with an
// error instead of blocking. The deadline applies to all future and pending
// I/O, not just the immediately following call to Read or Write.
// After a deadline has been exceeded, the connection can be refreshed
// by setting a deadline in the future.
//
// If the deadline is exceeded a call to Read or Write or to other I/O
// methods will return an error that wraps ErrDeadlineExceeded.
// This can be tested using errors.Is(err, os.ErrDeadlineExceeded).
// That error implements the Timeout method, and calling the Timeout
// method will return true, but there are other possible errors for which
// the Timeout will return true even if the deadline has not been exceeded.
//
// An idle timeout can be implemented by repeatedly extending
// the deadline after successful Read or Write calls.
//
// A zero value for t means I/O operations will not time out.
func (f *File) SetDeadline(t time.Time) error {
	return f.setDeadline(t)
}

// SetReadDeadline sets the deadline for future Read calls and any
// currently-blocked Read call.
// A zero value for t means Read will not time out.
// Not all files support setting deadlines; see SetDeadline.
func (f *File) SetReadDeadline(t time.Time) error {
	return f.setReadDeadline(t)
}

// SetWriteDeadline sets the deadline for any future Write calls and any
// currently-blocked Write call.
// Even if Write times out, it may return n > 0, indicating that
// some of the data was successfully written.
// A zero value for t means Write will not time out.
// Not all files support setting deadlines; see SetDeadline.
func (f *File) SetWriteDeadline(t time.Time) error {
	return f.setWriteDeadline(t)
}

// SyscallConn returns a raw file.
// This implements the syscall.Conn interface.
func (f *File) SyscallConn() (syscall.RawConn, error) {
	/*
		if err := f.checkValid("SyscallConn"); err != nil {
			return nil, err
		}
		return newRawConn(f)
	*/
	panic("todo: os.(*File).SyscallConn")
}

/* TODO(xsw):
// DirFS returns a file system (an fs.FS) for the tree of files rooted at the directory dir.
//
// Note that DirFS("/prefix") only guarantees that the Open calls it makes to the
// operating system will begin with "/prefix": DirFS("/prefix").Open("file") is the
// same as os.Open("/prefix/file"). So if /prefix/file is a symbolic link pointing outside
// the /prefix tree, then using DirFS does not stop the access any more than using
// os.Open does. Additionally, the root of the fs.FS returned for a relative path,
// DirFS("prefix"), will be affected by later calls to Chdir. DirFS is therefore not
// a general substitute for a chroot-style security mechanism when the directory tree
// contains arbitrary content.
//
// The directory dir must not be "".
//
// The result implements [io/fs.StatFS], [io/fs.ReadFileFS] and
// [io/fs.ReadDirFS].
func DirFS(dir string) fs.FS {
	return dirFS(dir)
}

// containsAny reports whether any bytes in chars are within s.
func containsAny(s, chars string) bool {
	for i := 0; i < len(s); i++ {
		for j := 0; j < len(chars); j++ {
			if s[i] == chars[j] {
				return true
			}
		}
	}
	return false
}

type dirFS string

func (dir dirFS) Open(name string) (fs.File, error) {
	fullname, err := dir.join(name)
	if err != nil {
		return nil, &PathError{Op: "stat", Path: name, Err: err}
	}
	f, err := Open(fullname)
	if err != nil {
		// DirFS takes a string appropriate for GOOS,
		// while the name argument here is always slash separated.
		// dir.join will have mixed the two; undo that for
		// error reporting.
		err.(*PathError).Path = name
		return nil, err
	}
	return f, nil
}

// The ReadFile method calls the [ReadFile] function for the file
// with the given name in the directory. The function provides
// robust handling for small files and special file systems.
// Through this method, dirFS implements [io/fs.ReadFileFS].
func (dir dirFS) ReadFile(name string) ([]byte, error) {
	fullname, err := dir.join(name)
	if err != nil {
		return nil, &PathError{Op: "readfile", Path: name, Err: err}
	}
	return ReadFile(fullname)
}

// ReadDir reads the named directory, returning all its directory entries sorted
// by filename. Through this method, dirFS implements [io/fs.ReadDirFS].
func (dir dirFS) ReadDir(name string) ([]DirEntry, error) {
	fullname, err := dir.join(name)
	if err != nil {
		return nil, &PathError{Op: "readdir", Path: name, Err: err}
	}
	return ReadDir(fullname)
}

func (dir dirFS) Stat(name string) (fs.FileInfo, error) {
	fullname, err := dir.join(name)
	if err != nil {
		return nil, &PathError{Op: "stat", Path: name, Err: err}
	}
	f, err := Stat(fullname)
	if err != nil {
		// See comment in dirFS.Open.
		err.(*PathError).Path = name
		return nil, err
	}
	return f, nil
}

// join returns the path for name in dir.
func (dir dirFS) join(name string) (string, error) {
	if dir == "" {
		return "", errors.New("os: DirFS with empty root")
	}
	if !fs.ValidPath(name) {
		return "", ErrInvalid
	}
	name, err := safefilepath.FromFS(name)
	if err != nil {
		return "", ErrInvalid
	}
	if IsPathSeparator(dir[len(dir)-1]) {
		return string(dir) + name, nil
	}
	return string(dir) + string(PathSeparator) + name, nil
}
*/

// ReadFile reads the named file and returns the contents.
// A successful call returns err == nil, not err == EOF.
// Because ReadFile reads the whole file, it does not treat an EOF from Read
// as an error to be reported.
func ReadFile(name string) ([]byte, error) {
	f, err := Open(name)
	if err != nil {
		return nil, err
	}

	var size int
	if info, err := f.Stat(); err == nil {
		size64 := info.Size()
		if int64(int(size64)) == size64 {
			size = int(size64)
		}
	}
	size++ // one byte for final read at EOF

	// If a file claims a small size, read at least 512 bytes.
	// In particular, files in Linux's /proc claim size 0 but
	// then do not work right if read in small pieces,
	// so an initial read of 1 byte would not work correctly.
	if size < 512 {
		size = 512
	}

	data := make([]byte, 0, size)
	for {
		if len(data) >= cap(data) {
			d := append(data[:cap(data)], 0)
			data = d[:len(data)]
		}
		n, err := f.Read(data[len(data):cap(data)])
		data = data[:len(data)+n]
		if err != nil {
			if err == io.EOF {
				err = nil
			}
			f.Close()
			return data, err
		}
	}
}

// WriteFile writes data to the named file, creating it if necessary.
// If the file does not exist, WriteFile creates it with permissions perm (before umask);
// otherwise WriteFile truncates it before writing, without changing permissions.
// Since WriteFile requires multiple system calls to complete, a failure mid-operation
// can leave the file in a partially written state.
func WriteFile(name string, data []byte, perm FileMode) error {
	f, err := OpenFile(name, O_WRONLY|O_CREATE|O_TRUNC, perm)
	if err != nil {
		return err
	}
	_, err = f.Write(data)
	if err1 := f.Close(); err1 != nil && err == nil {
		err = err1
	}
	return err
}
