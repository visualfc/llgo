// LITTEST
package main

// CHECK: {{^}}@0 = private unnamed_addr constant [4 x i8] c"loop", align 1{{$}}

func main() {
	for i := 0; i < 3; i++ {
		defer println("loop", i)
	}
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/deferloop.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testgo/deferloop.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testgo/deferloop.init$guard", align 1
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/deferloop.main"(){{.*}} {
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
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/deferloop.main", %_llgo_6), ptr %6, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.SetThreadDefer"(ptr %2)
// CHECK-NEXT:   %7 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 1
// CHECK-NEXT:   %8 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 3
// CHECK-NEXT:   %9 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 4
// CHECK-NEXT:   %10 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 5
// CHECK-NEXT:   store ptr null, ptr %10, align 8
// CHECK-NEXT:   %11 = call i32 @sigsetjmp(ptr %1, i32 0)
// CHECK-NEXT:   %12 = icmp eq i32 %11, 0
// CHECK-NEXT:   br i1 %12, label %_llgo_5, label %_llgo_8
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_2, %_llgo_5
// CHECK-NEXT:   %13 = phi i64 [ 0, %_llgo_5 ], [ %21, %_llgo_2 ]
// CHECK-NEXT:   %14 = icmp slt i64 %13, 3
// CHECK-NEXT:   br i1 %14, label %_llgo_2, label %_llgo_3
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1
// CHECK-NEXT:   %15 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %16 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 40)
// CHECK-NEXT:   %17 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String", i64 }, ptr %16, i32 0, i32 0
// CHECK-NEXT:   store ptr %15, ptr %17, align 8
// CHECK-NEXT:   %18 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String", i64 }, ptr %16, i32 0, i32 1
// CHECK-NEXT:   store i64 0, ptr %18, align 8
// CHECK-NEXT:   %19 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String", i64 }, ptr %16, i32 0, i32 2
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @0, i64 4 }, ptr %19, align 8
// CHECK-NEXT:   %20 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String", i64 }, ptr %16, i32 0, i32 3
// CHECK-NEXT:   store i64 %13, ptr %20, align 8
// CHECK-NEXT:   store ptr %16, ptr %10, align 8
// CHECK-NEXT:   %21 = add i64 %13, 1
// CHECK-NEXT:   br label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_1
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/deferloop.main", %_llgo_9), ptr %9, align 8
// CHECK-NEXT:   br label %_llgo_6
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_4:                                          ; preds = %_llgo_7
// CHECK-NEXT:   ret void
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_5:                                          ; preds = %_llgo_0
// CHECK-NEXT:   br label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_6:                                          ; preds = %_llgo_8, %_llgo_3
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/deferloop.main", %_llgo_7), ptr %8, align 8
// CHECK-NEXT:   %22 = load i64, ptr %7, align 8
// CHECK-NEXT:   br label %_llgo_10
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_7:                                          ; preds = %_llgo_8, %_llgo_11
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Rethrow"(ptr %0)
// CHECK-NEXT:   br label %_llgo_4
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_8:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/deferloop.main", %_llgo_7), ptr %9, align 8
// CHECK-NEXT:   %23 = load ptr, ptr %8, align 8
// CHECK-NEXT:   indirectbr ptr %23, [label %_llgo_7, label %_llgo_6]
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_9:                                          ; preds = %_llgo_11
// CHECK-NEXT:   ret void
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_10:                                         ; preds = %_llgo_16, %_llgo_6
// CHECK-NEXT:   %24 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %25 = icmp ne ptr %24, null
// CHECK-NEXT:   br i1 %25, label %_llgo_12, label %_llgo_11
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_11:                                         ; preds = %_llgo_13, %_llgo_10
// CHECK-NEXT:   %26 = load %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, align 8
// CHECK-NEXT:   %27 = extractvalue %"{{.*}}/runtime/internal/runtime.Defer" %26, 2
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.SetThreadDefer"(ptr %27)
// CHECK-NEXT:   %28 = load ptr, ptr %9, align 8
// CHECK-NEXT:   indirectbr ptr %28, [label %_llgo_7, label %_llgo_9]
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_12:                                         ; preds = %_llgo_10
// CHECK-NEXT:   %29 = load { ptr, i64 }, ptr %24, align 8
// CHECK-NEXT:   %30 = extractvalue { ptr, i64 } %29, 1
// CHECK-NEXT:   br label %_llgo_13
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_13:                                         ; preds = %_llgo_12
// CHECK-NEXT:   %31 = icmp eq i64 %30, 0
// CHECK-NEXT:   br i1 %31, label %_llgo_14, label %_llgo_11
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_14:                                         ; preds = %_llgo_13
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/deferloop.main", %_llgo_6), ptr %8, align 8
// CHECK-NEXT:   %32 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %33 = icmp ne ptr %32, null
// CHECK-NEXT:   br i1 %33, label %_llgo_15, label %_llgo_16
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_15:                                         ; preds = %_llgo_14
// CHECK-NEXT:   %34 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %35 = load { ptr, i64, %"{{.*}}/runtime/internal/runtime.String", i64 }, ptr %34, align 8
// CHECK-NEXT:   %36 = extractvalue { ptr, i64, %"{{.*}}/runtime/internal/runtime.String", i64 } %35, 0
// CHECK-NEXT:   store ptr %36, ptr %10, align 8
// CHECK-NEXT:   %37 = extractvalue { ptr, i64, %"{{.*}}/runtime/internal/runtime.String", i64 } %35, 2
// CHECK-NEXT:   %38 = extractvalue { ptr, i64, %"{{.*}}/runtime/internal/runtime.String", i64 } %35, 3
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.FreeDeferNode"(ptr %34)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" %37)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintInt"(i64 %38)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_16
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_16:                                         ; preds = %_llgo_15, %_llgo_14
// CHECK-NEXT:   br label %_llgo_10
// CHECK-NEXT: }
