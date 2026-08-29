// LITTEST
package main

import "github.com/xgo-dev/llgo/cl/_testlibc/once/local"

// CHECK-LABEL: define void @main.init(){{.*}} {
// pthread_once_t is a named aggregate on Darwin and i32 on Linux. In both
// cases, preserve the association from the runtime initializer to main.once.
// DARWIN-ARM64: [[ONCE_INIT:%[0-9]+]] = load [[ONCE_TYPE:%"[^"]*/_testlibc/once/local.Once"]], ptr @llgoSyncOnceInitVal
// LINUX-AMD64: [[ONCE_INIT:%[0-9]+]] = load [[ONCE_TYPE:i32]], ptr @llgoSyncOnceInitVal
// CHECK-NEXT: store [[ONCE_TYPE]] [[ONCE_INIT]], ptr @main.once
// pthread_once accepts a bare C callback on POSIX hosts.
// CHECK-LABEL: define void @main.runOnce(){{.*}} {
// DARWIN: call i32 @pthread_once(ptr @main.once, ptr @"main.runOnce$1")
// LINUX: call i32 @pthread_once(ptr @main.once, ptr @"main.runOnce$1")

var once local.Once = local.OnceInit

func runOnce() {
	once.Do(func() {
		println("Do once")
	})
}

func main() {
	runOnce()
	runOnce()
}
