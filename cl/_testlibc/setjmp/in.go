// LITTEST darwin/arm64 linux/amd64 windows/386 windows/amd64 windows/arm64
package main

import (
	"github.com/goplus/lib/c"
)

// CHECK-LABEL: define void @main.main(){{.*}} {
// The libc ABI controls the sigjmp_buf size and libc symbol spellings.
// DARWIN-ARM64: [[JMPBUF:%[0-9]+]] = alloca i8, i64 196
// LINUX-AMD64: [[JMPBUF:%[0-9]+]] = alloca i8, i64 200
// WINDOWS-386: [[JMPBUF:%[0-9]+]] = alloca i8, i32 64, align 16
// WINDOWS-AMD64: [[JMPBUF:%[0-9]+]] = alloca i8, i64 256, align 16
// WINDOWS-ARM64: [[JMPBUF:%[0-9]+]] = alloca i8, i64 192, align 16
// DARWIN-ARM64-NEXT: [[RET:%[0-9]+]] = call i32 @sigsetjmp(ptr [[JMPBUF]], i32 0)
// LINUX-AMD64-NEXT: [[RET:%[0-9]+]] = call i32 @__sigsetjmp(ptr [[JMPBUF]], i32 0)
// WINDOWS-386-NEXT: [[RET:%[0-9]+]] = call i32 @_setjmp3(ptr [[JMPBUF]], i32 0)
// WINDOWS-AMD64-NEXT: [[RET:%[0-9]+]] = call i32 @_setjmpex(ptr [[JMPBUF]], ptr null)
// WINDOWS-ARM64-NEXT: [[RET:%[0-9]+]] = call i32 @llgo_setjmp(ptr [[JMPBUF]])
// CHECK-NEXT: [[FIRST:%[0-9]+]] = icmp eq i32 [[RET]], 0
// CHECK-NEXT: br i1 [[FIRST]], label %{{[^,]+}}, label %{{[^ ]+}}
// CHECK: {{^_llgo_[0-9]+:}}
// DARWIN-ARM64: [[STDERR:%[0-9]+]] = load ptr, ptr @__stderrp
// LINUX-AMD64: [[STDERR:%[0-9]+]] = load ptr, ptr @stderr
// WINDOWS: [[STDERR:%[0-9]+]] = load ptr, ptr @"github.com/goplus/lib/c.Stderr"
// CHECK-NEXT: call i32 (ptr, ptr, ...) @fprintf(ptr [[STDERR]], ptr @{{[0-9]+}}, ptr getelementptr (i8, ptr getelementptr (i8, ptr @{{[0-9]+}}, i{{32|64}} 1), i{{32|64}} 1))
// DARWIN-NEXT: call void @siglongjmp(ptr [[JMPBUF]], i32 1)
// LINUX-NEXT: call void @siglongjmp(ptr [[JMPBUF]], i32 1)
// WINDOWS-386-NEXT: call void @longjmp(ptr [[JMPBUF]], i32 1)
// WINDOWS-AMD64-NEXT: call void @longjmp(ptr [[JMPBUF]], i32 1)
// WINDOWS-ARM64-NEXT: call void @llgo_longjmp(ptr [[JMPBUF]], i32 1)
// CHECK: {{^_llgo_[0-9]+:}}
// CHECK: [[PRINT_RET:%[0-9]+]] = sext i32 [[RET]] to i64
// CHECK-NEXT: call void @"{{.*}}PrintInt"(i64 [[PRINT_RET]])

func main() {
	jb := c.AllocaSigjmpBuf()
	switch ret := c.Sigsetjmp(jb, 0); ret {
	case 0:
		cstr := c.Str("??Hello, setjmp!\n")
		c.Fprintf(c.Stderr, c.Str("%s"), c.Advance(c.Pointer(c.Advance(cstr, 1)), 1))
		c.Siglongjmp(jb, 1)
	default:
		println("exception:", ret)
	}
}
