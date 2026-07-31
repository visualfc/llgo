// LITTEST
package main

import "net/textproto"

// CHECK-LABEL: define void @main.main(){{.*}} {
func main() {
	// CHECK: %0 = call %"{{.*}}/runtime/internal/runtime.String" @"net/textproto.CanonicalMIMEHeaderKey"(%"{{.*}}/runtime/internal/runtime.String" { ptr @0, i64 4 })
	println(textproto.CanonicalMIMEHeaderKey("host"))
}
