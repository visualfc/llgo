//go:build windows && arm64

package setjmp

// Microsoft UCRT jmp_buf is 24 64-bit slots on ARM64.
const (
	SigjmpBufSize = 192
	JmpBufSize    = 192
)
