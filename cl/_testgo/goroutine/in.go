// LITTEST
package main

// CHECK-LABEL: define void @"{{.*}}goroutine.main"(){{.*}} {
func main() {
	// CHECK: call ptr @"{{.*}}AllocZ"(i64 1)
	// CHECK: store i1 false, ptr %0, align 1
	// CHECK: call ptr @"{{.*}}AllocRoot"(i64 16)
	// CHECK: call void @"{{.*}}NewProc"(ptr @"{{.*}}goroutine._llgo_routine$1", ptr %1, i64 0)
	done := false
	go println("hello")
	go func(s string) {
		// CHECK: call ptr @"{{.*}}AllocU"(i64 8)
		// CHECK: { ptr @"{{.*}}goroutine.main$1", ptr undef }
		// CHECK: call ptr @"{{.*}}AllocRoot"(i64 32)
		// CHECK: call void @"{{.*}}NewProc"(ptr @"{{.*}}goroutine._llgo_routine$2", ptr {{%[0-9]+}}, i64 0)
		// CHECK: call void @"{{.*}}PrintString"(%"{{.*}}String" { ptr @2, i64 1 })
		// CHECK: ret void
		// CHECK-LABEL: define void @"{{.*}}goroutine.main$1"(ptr %0, %"{{.*}}String" %1){{.*}} {
		// CHECK-NEXT: _llgo_0:
		// CHECK-NEXT:   call void @"{{.*}}PrintString"(%"{{.*}}String" %1)
		// CHECK-NEXT:   call void @"{{.*}}PrintByte"(i8 10)
		// CHECK-NEXT:   %2 = load { ptr }, ptr %0, align 8
		// CHECK-NEXT:   %3 = extractvalue { ptr } %2, 0
		// CHECK-NEXT:   store i1 true, ptr %3, align 1
		// CHECK-NEXT:   ret void
		println(s)
		done = true
	}("Hello, goroutine")
	for !done {
		print(".")
	}
}

// CHECK-LABEL: define ptr @"{{.*}}goroutine._llgo_routine$1"(ptr %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = alloca %"{{.*}}LocalContext", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %1, i8 0, i64 8, i1 false)
// CHECK-NEXT:   %2 = call i64 @"{{.*}}EnterLocalContext"(ptr %1)
// CHECK-NEXT:   %3 = load { %"{{.*}}String" }, ptr %0, align 8
// CHECK-NEXT:   %4 = extractvalue { %"{{.*}}String" } %3, 0
// CHECK-NEXT:   call void @"{{.*}}FreeRoot"(ptr %0)
// CHECK-NEXT:   call void @"{{.*}}PrintString"(%"{{.*}}String" %4)
// CHECK-NEXT:   call void @"{{.*}}PrintByte"(i8 10)
// CHECK-NEXT:   call void @"{{.*}}LeaveLocalContext"(ptr %1, i64 %2)
// CHECK-NEXT:   ret ptr null

// CHECK-LABEL: define ptr @"{{.*}}goroutine._llgo_routine$2"(ptr %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = alloca %"{{.*}}LocalContext", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %1, i8 0, i64 8, i1 false)
// CHECK-NEXT:   %2 = call i64 @"{{.*}}EnterLocalContext"(ptr %1)
// CHECK-NEXT:   %3 = load { { ptr, ptr }, %"{{.*}}String" }, ptr %0, align 8
// CHECK-NEXT:   %4 = extractvalue { { ptr, ptr }, %"{{.*}}String" } %3, 0
// CHECK-NEXT:   %5 = extractvalue { { ptr, ptr }, %"{{.*}}String" } %3, 1
// CHECK-NEXT:   call void @"{{.*}}FreeRoot"(ptr %0)
// CHECK-NEXT:   %6 = extractvalue { ptr, ptr } %4, 1
// CHECK-NEXT:   %7 = extractvalue { ptr, ptr } %4, 0
// CHECK-NEXT:   call void %7(ptr %6, %"{{.*}}String" %5)
// CHECK-NEXT:   call void @"{{.*}}LeaveLocalContext"(ptr %1, i64 %2)
// CHECK-NEXT:   ret ptr null
