//go:build windows && amd64

package setjmp

// Microsoft UCRT's register-save structure occupies 256 bytes on x64.
const (
	SigjmpBufSize = 256
	JmpBufSize    = 256
)
