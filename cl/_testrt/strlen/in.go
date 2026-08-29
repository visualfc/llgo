// LITTEST
// Scope: common
package main

import "C"
import _ "unsafe"

//go:linkname printf C.printf
func printf(format *int8, __llgo_va_list ...any) int32

//go:linkname strlen C.strlen
func strlen(str *int8) uintptr

var (
	hello     = [...]int8{'H', 'e', 'l', 'l', 'o', '\n', 0}
	lengthFmt = [...]int8{'L', 'e', 'n', 'g', 't', 'h', ' ', '%', 'z', 'u', '\n', 0}
	int32Fmt  = [...]int8{'I', 'n', 't', '3', '2', ' ', '%', 'd', '\n', 0}
	int64Fmt  = [...]int8{'I', 'n', 't', '6', '4', ' ', '%', 'l', 'l', 'd', '\n', 0}
)

// CHECK-LABEL: define void @main.main(){{.*}} {
// CHECK: call i32 (ptr, ...) @printf(ptr @main.hello)
// CHECK-NEXT: [[STRLEN:%[0-9]+]] = call [[SIZE_T:i[0-9]+]] @strlen(ptr @main.hello)
// CHECK-NEXT: call i32 (ptr, ...) @printf(ptr @main.lengthFmt, [[SIZE_T]] [[STRLEN]])
// CHECK-NEXT: call i32 (ptr, ...) @printf(ptr @main.int32Fmt, i32 6)
// CHECK-NEXT: call i32 (ptr, ...) @printf(ptr @main.int64Fmt, i64 100)
func main() {
	text := &hello[0]
	printf(text)
	printf(&lengthFmt[0], strlen(text))
	printf(&int32Fmt[0], int32(6))
	printf(&int64Fmt[0], int64(100))
}
