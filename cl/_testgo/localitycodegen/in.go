// LITTEST
package main

// CHECK-DAG: @main.scalar = thread_local global i64 0
// CHECK-DAG: @main.__llgo_local_cache = thread_local global i64 0
// CHECK-DAG: @"main.__llgo_tls_init$guard" = thread_local global i8 0
// CHECK-DAG: @"main.__llgo_tls_init$failure_cache" = thread_local global i64 0
// CHECK-NOT: RegisterLocalRoot
// CHECK-NOT: localitycodegen.pointer" = thread_local
// CHECK-NOT: localitycodegen.initialized" = thread_local

// CHECK-LABEL: define ptr @main.__llgo_local_block()
// CHECK: load i64, ptr @main.__llgo_local_cache
// CHECK: icmp ne i64
// CHECK: ret ptr
// CHECK: call ptr @"{{.*}}runtime.LocalPackage"(ptr @main.__llgo_local_cache, i64 16, i64 8)
// CHECK: ret ptr

// CHECK-LABEL: define void @main.__llgo_tls_init()
// CHECK: call void @main.__llgo_local_init_0()

// CHECK-LABEL: define void @"main.__llgo_tls_init$ensure"()
// CHECK: load i8, ptr
// CHECK: call void @"{{.*}}runtime.EnsureLocalInitializer"(ptr @"main.__llgo_tls_init$guard", ptr @"main.__llgo_tls_init$failure_cache"

// CHECK-LABEL: define ptr @{{"?ExportedLocality"?}}()
// CHECK: call i64 @"{{.*}}EnterLocalContext"
// CHECK: call ptr @main.__llgo_local_block()
// CHECK: call void @"{{.*}}LeaveLocalContext"
// CHECK: ret ptr

// CHECK-LABEL: define void @main.init()
// CHECK: store i8 2, ptr
// CHECK: call ptr @main.newPointer()
// CHECK: call void @"main.__llgo_tls_init$ensure"()
// CHECK: call ptr @main.__llgo_local_block()

// CHECK-LABEL: define { i64, ptr, ptr } @main.values()
// CHECK: call void @"main.__llgo_tls_init$ensure"()
// CHECK: load i64, ptr @main.scalar
// CHECK: call ptr @main.__llgo_local_block()
// CHECK: load ptr, ptr
// CHECK: load ptr, ptr

// CHECK-LABEL: define ptr @"main._llgo_routine$1"(ptr %0)
// CHECK: alloca %"{{.*}}LocalContext", align 8
// CHECK: call i64 @"{{.*}}EnterLocalContext"
// CHECK: call void @"{{.*}}LeaveLocalContext"

var backing int

func newPointer() *int {
	return &backing
}

//llgo:tls
var scalar int

//llgo:gls
var pointer *int

//llgo:tls
var initialized = newPointer()

func values() (int, *int, *int) {
	return scalar, pointer, initialized
}

//export ExportedLocality
func ExportedLocality() *int {
	return pointer
}

func main() {
	_, _, _ = values()
	go values()
}
