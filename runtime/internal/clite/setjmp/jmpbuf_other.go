//go:build !((linux || darwin) && (amd64 || arm64)) && !windows && !baremetal

package setjmp

const (
	SigjmpBufSize = 200
	JmpBufSize    = 200
)
