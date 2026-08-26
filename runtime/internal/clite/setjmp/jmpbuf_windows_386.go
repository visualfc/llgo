//go:build windows && 386

package setjmp

// Microsoft UCRT jmp_buf is 16 32-bit integers on x86.
const (
	SigjmpBufSize = 64
	JmpBufSize    = 64
)
