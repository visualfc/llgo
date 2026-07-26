// LITTEST
package main

/*
#include "in.h"
*/
import "C"
import "fmt"

// CHECK: {{^}}@2 = private unnamed_addr constant [19 x i8] c"test_structs failed", align 1{{$}}

func main() {
	r := C.test_structs(&C.s4{a: 1}, &C.s8{a: 1, b: 2}, &C.s12{a: 1, b: 2, c: 3}, &C.s16{a: 1, b: 2, c: 3, d: 4}, &C.s20{a: 1, b: 2, c: 3, d: 4, e: 5})
	fmt.Println(r)
	if r != 35 {
		panic("test_structs failed")
	}
}

// CHECK-LABEL: define i32 @"{{.*}}/cl/_testgo/cgocfiles._Cfunc_test_structs"(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %5 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 8)
// CHECK-NEXT:   %6 = load ptr, ptr @"{{.*}}/cl/_testgo/cgocfiles._cgo_{{.*}}_Cfunc_test_structs", align 8
// CHECK-NEXT:   %7 = load ptr, ptr %6, align 8
// CHECK-NEXT:   %8 = call i32 %7(ptr %0, ptr %1, ptr %2, ptr %3, ptr %4)
// CHECK-NEXT:   ret i32 %8
// CHECK-NEXT: }

// CHECK-LABEL: define ptr @"{{.*}}/cl/_testgo/cgocfiles._Cgo_ptr"(ptr %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   ret ptr %0
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/cgocfiles.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testgo/cgocfiles.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testgo/cgocfiles.init$guard", align 1
// CHECK-NEXT:   call void @syscall.init()
// CHECK-NEXT:   call void @fmt.init()
// CHECK-NEXT:   store ptr @_cgo_{{.*}}_Cfunc_test_structs, ptr @"{{.*}}/cl/_testgo/cgocfiles._cgo_{{.*}}_Cfunc_test_structs", align 8
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/cgocfiles.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 4)
// CHECK-NEXT:   %1 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___3", ptr %0, i32 0, i32 0
// CHECK-NEXT:   store i32 1, ptr %1, align 4
// CHECK-NEXT:   %2 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 8)
// CHECK-NEXT:   %3 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___4", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %4 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___4", ptr %2, i32 0, i32 1
// CHECK-NEXT:   store i32 1, ptr %3, align 4
// CHECK-NEXT:   store i32 2, ptr %4, align 4
// CHECK-NEXT:   %5 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 12)
// CHECK-NEXT:   %6 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___0", ptr %5, i32 0, i32 0
// CHECK-NEXT:   %7 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___0", ptr %5, i32 0, i32 1
// CHECK-NEXT:   %8 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___0", ptr %5, i32 0, i32 2
// CHECK-NEXT:   store i32 1, ptr %6, align 4
// CHECK-NEXT:   store i32 2, ptr %7, align 4
// CHECK-NEXT:   store i32 3, ptr %8, align 4
// CHECK-NEXT:   %9 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 16)
// CHECK-NEXT:   %10 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___1", ptr %9, i32 0, i32 0
// CHECK-NEXT:   %11 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___1", ptr %9, i32 0, i32 1
// CHECK-NEXT:   %12 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___1", ptr %9, i32 0, i32 2
// CHECK-NEXT:   %13 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___1", ptr %9, i32 0, i32 3
// CHECK-NEXT:   store i32 1, ptr %10, align 4
// CHECK-NEXT:   store i32 2, ptr %11, align 4
// CHECK-NEXT:   store i32 3, ptr %12, align 4
// CHECK-NEXT:   store i32 4, ptr %13, align 4
// CHECK-NEXT:   %14 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 20)
// CHECK-NEXT:   %15 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___2", ptr %14, i32 0, i32 0
// CHECK-NEXT:   %16 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___2", ptr %14, i32 0, i32 1
// CHECK-NEXT:   %17 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___2", ptr %14, i32 0, i32 2
// CHECK-NEXT:   %18 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___2", ptr %14, i32 0, i32 3
// CHECK-NEXT:   %19 = getelementptr inbounds %"{{.*}}/cl/_testgo/cgocfiles._Ctype_struct___2", ptr %14, i32 0, i32 4
// CHECK-NEXT:   store i32 1, ptr %15, align 4
// CHECK-NEXT:   store i32 2, ptr %16, align 4
// CHECK-NEXT:   store i32 3, ptr %17, align 4
// CHECK-NEXT:   store i32 4, ptr %18, align 4
// CHECK-NEXT:   store i32 5, ptr %19, align 4
// CHECK-NEXT:   %20 = call i32 @"{{.*}}/cl/_testgo/cgocfiles._Cfunc_test_structs"(ptr %0, ptr %2, ptr %5, ptr %9, ptr %14)
// CHECK-NEXT:   %21 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 16)
// CHECK-NEXT:   %22 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.eface", ptr %21, i64 0
// CHECK-NEXT:   %23 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 4)
// CHECK-NEXT:   store i32 %20, ptr %23, align 4
// CHECK-NEXT:   %24 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_{{.*}}/cl/_testgo/cgocfiles._Ctype_int", ptr undef }, ptr %23, 1
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.eface" %24, ptr %22, align 8
// CHECK-NEXT:   %25 = insertvalue %"{{.*}}/runtime/internal/runtime.Slice" undef, ptr %21, 0
// CHECK-NEXT:   %26 = insertvalue %"{{.*}}/runtime/internal/runtime.Slice" %25, i64 1, 1
// CHECK-NEXT:   %27 = insertvalue %"{{.*}}/runtime/internal/runtime.Slice" %26, i64 1, 2
// CHECK-NEXT:   %28 = call { i64, %"{{.*}}/runtime/internal/runtime.iface" } @fmt.Println(%"{{.*}}/runtime/internal/runtime.Slice" %27)
// CHECK-NEXT:   %29 = icmp ne i32 %20, 35
// CHECK-NEXT:   br i1 %29, label %_llgo_1, label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %30 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @2, i64 19 }, ptr %30, align 8
// CHECK-NEXT:   %31 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %30, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %31)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
