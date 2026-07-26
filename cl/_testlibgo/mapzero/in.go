// LITTEST
package main

import "fmt"

// CHECK-LABEL: define void @"{{.*}}/cl/_testlibgo/mapzero.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testlibgo/mapzero.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testlibgo/mapzero.init$guard", align 1
// CHECK-NEXT:   call void @fmt.init()
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

var a = 0

// CHECK-LABEL: define void @"{{.*}}/cl/_testlibgo/mapzero.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.GetThreadDefer"()
// CHECK-NEXT:   %1 = alloca i8, i64 196, align 1
// CHECK-NEXT:   %2 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 48)
// CHECK-NEXT:   %3 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 0
// CHECK-NEXT:   store ptr %1, ptr %3, align 8
// CHECK-NEXT:   %4 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 1
// CHECK-NEXT:   store i64 0, ptr %4, align 8
// CHECK-NEXT:   %5 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 2
// CHECK-NEXT:   store ptr %0, ptr %5, align 8
// CHECK-NEXT:   %6 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 3
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testlibgo/mapzero.main", %_llgo_2), ptr %6, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.SetThreadDefer"(ptr %2)
// CHECK-NEXT:   %7 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 1
// CHECK-NEXT:   %8 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 3
// CHECK-NEXT:   %9 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 4
// CHECK-NEXT:   %10 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 5
// CHECK-NEXT:   store ptr null, ptr %10, align 8
// CHECK-NEXT:   %11 = call i32 @sigsetjmp(ptr %1, i32 0)
// CHECK-NEXT:   %12 = icmp eq i32 %11, 0
// CHECK-NEXT:   br i1 %12, label %_llgo_4, label %_llgo_5
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_3
// CHECK-NEXT:   ret void
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_5, %_llgo_4
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testlibgo/mapzero.main", %_llgo_3), ptr %8, align 8
// CHECK-NEXT:   %13 = load i64, ptr %7, align 8
// CHECK-NEXT:   call void @"{{.*}}/cl/_testlibgo/mapzero.main$1"()
// CHECK-NEXT:   %14 = load %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, align 8
// CHECK-NEXT:   %15 = extractvalue %"{{.*}}/runtime/internal/runtime.Defer" %14, 2
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.SetThreadDefer"(ptr %15)
// CHECK-NEXT:   %16 = load ptr, ptr %9, align 8
// CHECK-NEXT:   indirectbr ptr %16, [label %_llgo_3, label %_llgo_6]
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_5, %_llgo_2
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Rethrow"(ptr %0)
// CHECK-NEXT:   br label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_4:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %17 = load i64, ptr @"{{.*}}/cl/_testlibgo/mapzero.a", align 8
// CHECK-NEXT:   %18 = icmp slt i64 %17, 0
// CHECK-NEXT:   %19 = icmp uge i64 %17, 0
// CHECK-NEXT:   %20 = or i1 %19, %18
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.CheckIndexRange"(i1 %20, i64 %17, i1 true, i64 0)
// CHECK-NEXT:   %21 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 8)
// CHECK-NEXT:   store i64 0, ptr %21, align 8
// CHECK-NEXT:   %22 = call ptr @"{{.*}}/runtime/internal/runtime.MapAccess1"(ptr @"map[_llgo_int]_llgo_int", ptr null, ptr %21)
// CHECK-NEXT:   %23 = load i64, ptr %22, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintInt"(i64 %23)
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testlibgo/mapzero.main", %_llgo_6), ptr %9, align 8
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_5:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testlibgo/mapzero.main", %_llgo_3), ptr %9, align 8
// CHECK-NEXT:   %24 = load ptr, ptr %8, align 8
// CHECK-NEXT:   indirectbr ptr %24, [label %_llgo_3, label %_llgo_2]
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_6:                                          ; preds = %_llgo_2
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

func main() {
	// CHECK-LABEL: define void @"{{.*}}/cl/_testlibgo/mapzero.main$1"(){{.*}} {
	// CHECK-NEXT: _llgo_0:
	// CHECK-NEXT:   %0 = call %"{{.*}}/runtime/internal/runtime.eface" @"{{.*}}/runtime/internal/runtime.Recover"()
	// CHECK-NEXT:   %1 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 16)
	// CHECK-NEXT:   %2 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.eface", ptr %1, i64 0
	// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.eface" %0, ptr %2, align 8
	// CHECK-NEXT:   %3 = insertvalue %"{{.*}}/runtime/internal/runtime.Slice" undef, ptr %1, 0
	// CHECK-NEXT:   %4 = insertvalue %"{{.*}}/runtime/internal/runtime.Slice" %3, i64 1, 1
	// CHECK-NEXT:   %5 = insertvalue %"{{.*}}/runtime/internal/runtime.Slice" %4, i64 1, 2
	// CHECK-NEXT:   %6 = call { i64, %"{{.*}}/runtime/internal/runtime.iface" } @fmt.Println(%"{{.*}}/runtime/internal/runtime.Slice" %5)
	// CHECK-NEXT:   ret void
	// CHECK-NEXT: }

	defer func() {
		err := recover()
		fmt.Println(err)
	}()
	m := [0]map[int]int{}[a][0]
	print(m)
}

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.memequal64"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.memequal64"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.memequal8"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.memequal8"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }
