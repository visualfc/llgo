// LITTEST
package main

import (
	"unsafe"

	"github.com/goplus/lib/c"
)

//
//go:linkname cstr llgo.cstr
func cstr(string) *int8

//go:linkname fprintf C.fprintf
func fprintf(fp unsafe.Pointer, format *int8, __llgo_va_list ...any)

// CHECK: [[FPRINTF_FORMAT:@[0-9]+]] = private unnamed_addr constant [10 x i8] c"Hello %d\0A\00"
// CHECK-LABEL: define void @main.main(){{.*}} {
// DARWIN: [[STDERR:%[0-9]+]] = load ptr, ptr @__stderrp
// LINUX: [[STDERR:%[0-9]+]] = load ptr, ptr @stderr
// WINDOWS: [[STDERR:%[0-9]+]] = load ptr, ptr @"github.com/goplus/lib/c.Stderr"
// CHECK-NEXT: call void (ptr, ptr, ...) @fprintf(ptr [[STDERR]], ptr [[FPRINTF_FORMAT]], i64 100)
func main() {
	fprintf(unsafe.Pointer(c.Stderr), cstr("Hello %d\n"), 100)
}
