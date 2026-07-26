// LITTEST
package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/sync/atomic"
)

// CHECK: {{^}}@0 = private unnamed_addr constant [12 x i8] c"store: %ld\0A\00", align 1{{$}}
// CHECK: {{^}}@1 = private unnamed_addr constant [18 x i8] c"ret: %ld, v: %ld\0A\00", align 1{{$}}
// CHECK: {{^}}@2 = private unnamed_addr constant [25 x i8] c"ret: %ld vs 100, v: %ld\0A\00", align 1{{$}}
// CHECK: {{^}}@3 = private unnamed_addr constant [25 x i8] c"ret: %ld vs 101, v: %ld\0A\00", align 1{{$}}
// CHECK: {{^}}@4 = private unnamed_addr constant [18 x i8] c"ret: %ld, v: %ld\0A\00", align 1{{$}}

func main() {
	var v int64

	atomic.Store(&v, 100)
	c.Printf(c.Str("store: %ld\n"), atomic.Load(&v))
	ret := atomic.Add(&v, 1)
	c.Printf(c.Str("ret: %ld, v: %ld\n"), ret, v)

	ret, _ = atomic.CompareAndExchange(&v, 100, 102)
	c.Printf(c.Str("ret: %ld vs 100, v: %ld\n"), ret, v)

	ret, _ = atomic.CompareAndExchange(&v, 101, 102)
	c.Printf(c.Str("ret: %ld vs 101, v: %ld\n"), ret, v)

	ret = atomic.Sub(&v, 1)
	c.Printf(c.Str("ret: %ld, v: %ld\n"), ret, v)
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testlibc/atomic.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testlibc/atomic.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testlibc/atomic.init$guard", align 1
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testlibc/atomic.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 8)
// CHECK-NEXT:   store atomic i64 100, ptr %0 seq_cst, align 8
// CHECK-NEXT:   %1 = load atomic i64, ptr %0 seq_cst, align 8
// CHECK-NEXT:   %2 = call i32 (ptr, ...) @printf(ptr @0, i64 %1)
// CHECK-NEXT:   %3 = atomicrmw add ptr %0, i64 1 seq_cst, align 8
// CHECK-NEXT:   %4 = load i64, ptr %0, align 8
// CHECK-NEXT:   %5 = call i32 (ptr, ...) @printf(ptr @1, i64 %3, i64 %4)
// CHECK-NEXT:   %6 = cmpxchg ptr %0, i64 100, i64 102 seq_cst seq_cst, align 8
// CHECK-NEXT:   %7 = extractvalue { i64, i1 } %6, 0
// CHECK-NEXT:   %8 = extractvalue { i64, i1 } %6, 1
// CHECK-NEXT:   %9 = load i64, ptr %0, align 8
// CHECK-NEXT:   %10 = call i32 (ptr, ...) @printf(ptr @2, i64 %7, i64 %9)
// CHECK-NEXT:   %11 = cmpxchg ptr %0, i64 101, i64 102 seq_cst seq_cst, align 8
// CHECK-NEXT:   %12 = extractvalue { i64, i1 } %11, 0
// CHECK-NEXT:   %13 = extractvalue { i64, i1 } %11, 1
// CHECK-NEXT:   %14 = load i64, ptr %0, align 8
// CHECK-NEXT:   %15 = call i32 (ptr, ...) @printf(ptr @3, i64 %12, i64 %14)
// CHECK-NEXT:   %16 = atomicrmw sub ptr %0, i64 1 seq_cst, align 8
// CHECK-NEXT:   %17 = load i64, ptr %0, align 8
// CHECK-NEXT:   %18 = call i32 (ptr, ...) @printf(ptr @4, i64 %16, i64 %17)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
