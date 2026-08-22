// LITTEST
package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/pthread/sync"
)

// The C-backed Once implementation must retain a concrete callback rather
// than duplicating the closure body at each call site. POSIX lowers directly
// to pthread_once; Windows calls the shared INIT_ONCE adapter.
// CHECK-LABEL: define void @main.f(){{.*}} {
// DARWIN: call i32 @pthread_once(ptr @main.once, ptr @"main.f$1")
// LINUX: call i32 @pthread_once(ptr @main.once, ptr @"main.f$1")
// WINDOWS: call i32 @"github.com/goplus/lib/c/pthread/sync.(*Once).Do"(ptr @main.once, { ptr, ptr } { ptr @"main.f$1", ptr null })
// CHECK-NEXT: ret void
// CHECK-LABEL: define void @"main.f$1"(){{.*}} {
// CHECK: call i32 (ptr, ...) @printf(ptr @{{[0-9]+}})
// CHECK-NEXT: ret void
// CHECK-LABEL: define void @main.init(){{.*}} {
// pthread_once_t is a named aggregate on Darwin and i32 on Linux. In both
// cases, preserve the association from the runtime initializer to main.once.
// DARWIN-ARM64: [[ONCE_INIT:%[0-9]+]] = load [[ONCE_TYPE:%"github.com/goplus/lib/c/pthread/sync.Once"]], ptr @llgoSyncOnceInitVal
// LINUX-AMD64: [[ONCE_INIT:%[0-9]+]] = load [[ONCE_TYPE:i32]], ptr @llgoSyncOnceInitVal
// WINDOWS: [[ONCE_INIT:%[0-9]+]] = load [[ONCE_TYPE:%"github.com/goplus/lib/c/pthread/sync.Once"]], ptr @"github.com/goplus/lib/c/pthread/sync.OnceInit"
// CHECK-NEXT: store [[ONCE_TYPE]] [[ONCE_INIT]], ptr @main.once
// CHECK-LABEL: define void @main.main(){{.*}} {
// CHECK: [[PREFIX:%[0-9]+]] = call %"{{.*}}String" @"{{.*}}StringFrom"(ptr @{{[0-9]+}}, i64 9)
// CHECK-NEXT: call void @"{{.*}}PrintString"(%"{{.*}}String" [[PREFIX]])
// CHECK: [[WHOLE:%[0-9]+]] = call %"{{.*}}String" @"{{.*}}StringFromCStr"(ptr @{{[0-9]+}})
// CHECK-NEXT: call void @"{{.*}}PrintString"(%"{{.*}}String" [[WHOLE]])
// CHECK: call void @main.f()
// CHECK-NEXT: call void @main.f()

var once sync.Once = sync.OnceInit

func f() {
	once.Do(func() {
		c.Printf(c.Str("Do once\n"))
	})
}

func main() {
	println(c.GoString(c.Str("sync.Once demo\n"), 9))
	println(c.GoString(c.Str("sync.Once demo\n")))
	f()
	f()
}
