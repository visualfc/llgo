// LITTEST
package main

import "C"
import _ "unsafe"

//go:linkname printf C.printf
func printf(format *int8, __llgo_va_list ...any)

//go:linkname strlen C.strlen
func strlen(str *int8) C.int

var (
	hello     = [...]int8{'H', 'e', 'l', 'l', 'o', '\n', 0}
	lengthFmt = [...]int8{'L', 'e', 'n', 'g', 't', 'h', ' ', '%', 'd', '\n', 0}
	valueFmt  = [...]int8{'V', 'a', 'l', 'u', 'e', ' ', '%', 'd', '\n', 0}
)

// CHECK-LABEL: define void @main.main(){{.*}} {
// CHECK: call void (ptr, ...) @printf(ptr @main.hello)
// CHECK-NEXT: [[STRLEN:%[0-9]+]] = call i32 @strlen(ptr @main.hello)
// CHECK-NEXT: call void (ptr, ...) @printf(ptr @main.lengthFmt, i32 [[STRLEN]])
// CHECK-NEXT: call void (ptr, ...) @printf(ptr @main.valueFmt, i64 100)
func main() {
	text := &hello[0]
	printf(text)
	printf(&lengthFmt[0], strlen(text))
	printf(&valueFmt[0], 100)
}
