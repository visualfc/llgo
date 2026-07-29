// LITTEST
package C

// CHECK: {{^}}@llvm.compiler.used = appending global [2 x ptr] [ptr @Double, ptr @add], section "llvm.metadata"{{$}}

// CHECK-LABEL: define double @Double(double %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = alloca %"{{.*}}LocalContext", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %1, i8 0, i64 8, i1 false)
// CHECK-NEXT:   %2 = call i64 @"{{.*}}EnterLocalContext"(ptr %1)
// CHECK-NEXT:   %3 = fmul double 2.000000e+00, %0
// CHECK-NEXT:   call void @"{{.*}}LeaveLocalContext"(ptr %1, i64 %2)
// CHECK-NEXT:   ret double %3
// CHECK-NEXT: }
func Double(x float64) float64 {
	return 2 * x
}

// CHECK-LABEL: define i64 @add(i64 %0, i64 %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = alloca %"{{.*}}LocalContext", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %2, i8 0, i64 8, i1 false)
// CHECK-NEXT:   %3 = call i64 @"{{.*}}EnterLocalContext"(ptr %2)
// CHECK-NEXT:   %4 = call i64 @"{{.*}}.add"(i64 %0, i64 %1)
// CHECK-NEXT:   call void @"{{.*}}LeaveLocalContext"(ptr %2, i64 %3)
// CHECK-NEXT:   ret i64 %4
// CHECK-NEXT: }
func Xadd(a, b int) int {
	return add(a, b)
}

// CHECK-LABEL: define i64 @"{{.*}}.add"(i64 %0, i64 %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = add i64 %0, %1
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }
func add(a, b int) int {
	return a + b
}
