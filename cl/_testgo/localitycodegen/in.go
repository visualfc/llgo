// LITTEST
package main

// CHECK-DAG: @"{{.*}}localitycodegen.Scalar" = thread_local global i64 0
// CHECK-DAG: @"{{.*}}localitycodegen.__llgo_local_key" = global i8 0
// CHECK-DAG: @"{{.*}}localitycodegen.__llgo_tls_init$guard" = thread_local global i8 0
// CHECK-DAG: @"{{.*}}localitycodegen.__llgo_tls_init$failure" = global i8 0
// CHECK-NOT: RegisterLocalRoot
// CHECK-NOT: localitycodegen.Pointer" = thread_local
// CHECK-NOT: localitycodegen.Initialized" = thread_local

// CHECK-LABEL: define ptr @"{{.*}}localitycodegen.__llgo_local_block"()
// CHECK: call ptr @"{{.*}}runtime.LocalPackage"(ptr @"{{.*}}localitycodegen.__llgo_local_key", i64 16, i64 8)
// CHECK: ret ptr

// CHECK-LABEL: define void @"{{.*}}localitycodegen.__llgo_tls_init"()
// CHECK: call void @"{{.*}}localitycodegen.__llgo_local_init_0"()

// CHECK-LABEL: define void @"{{.*}}localitycodegen.__llgo_tls_init$ensure"()
// CHECK: load i8, ptr
// CHECK: call void @"{{.*}}runtime.EnsureLocalInitializer"(ptr @"{{.*}}localitycodegen.__llgo_tls_init$guard", ptr @"{{.*}}localitycodegen.__llgo_tls_init$failure"

// CHECK-LABEL: define ptr @{{"?ExportedLocality"?}}()
// CHECK: call i64 @"{{.*}}EnterLocalContext"
// CHECK: call ptr @"{{.*}}localitycodegen.__llgo_local_block"()
// CHECK: call void @"{{.*}}LeaveLocalContext"
// CHECK: ret ptr

// CHECK-LABEL: define void @"{{.*}}localitycodegen.init"()
// CHECK: store i8 2, ptr
// CHECK: call ptr @"{{.*}}localitycodegen.newPointer"()
// CHECK: call void @"{{.*}}localitycodegen.__llgo_tls_init$ensure"()
// CHECK: call ptr @"{{.*}}localitycodegen.__llgo_local_block"()

// CHECK-LABEL: define { i64, ptr, ptr } @"{{.*}}localitycodegen.values"()
// CHECK: call void @"{{.*}}localitycodegen.__llgo_tls_init$ensure"()
// CHECK: load i64, ptr @"{{.*}}localitycodegen.Scalar"
// CHECK: call ptr @"{{.*}}localitycodegen.__llgo_local_block"()
// CHECK: load ptr, ptr
// CHECK: load ptr, ptr

// CHECK-LABEL: define ptr @"{{.*}}localitycodegen._llgo_routine$1"(ptr %0)
// CHECK: alloca %"{{.*}}LocalContext", align 8
// CHECK: call i64 @"{{.*}}EnterLocalContext"
// CHECK: call void @"{{.*}}LeaveLocalContext"

var backing int

func newPointer() *int {
	return &backing
}

//llgo:tls
var Scalar int

//llgo:gls
var Pointer *int

//llgo:tls
var Initialized = newPointer()

func values() (int, *int, *int) {
	return Scalar, Pointer, Initialized
}

//export ExportedLocality
func ExportedLocality() *int {
	return Pointer
}

func main() {
	_, _, _ = values()
	go values()
}
