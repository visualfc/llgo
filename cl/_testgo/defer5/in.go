// LITTEST
package main

// CHECK: {{^}}@0 = private unnamed_addr constant [1 x i8] c"A", align 1{{$}}
// CHECK: {{^}}@1 = private unnamed_addr constant [1 x i8] c"B", align 1{{$}}
// CHECK: {{^}}@2 = private unnamed_addr constant [13 x i8] c"panic in main", align 1{{$}}
// CHECK: {{^}}@4 = private unnamed_addr constant [10 x i8] c"in defer 1", align 1{{$}}
// CHECK: {{^}}@5 = private unnamed_addr constant [16 x i8] c"panic in defer 1", align 1{{$}}
// CHECK: {{^}}@6 = private unnamed_addr constant [10 x i8] c"in defer 2", align 1{{$}}
// CHECK: {{^}}@7 = private unnamed_addr constant [16 x i8] c"panic in defer 2", align 1{{$}}

func main() {
	defer println("A")
	defer func() {
		if e := recover(); e != nil {
			println("in defer 1")
			panic("panic in defer 1")
		}
	}()
	defer func() {
		println("in defer 2")
		panic("panic in defer 2")
	}()
	defer println("B")
	panic("panic in main")
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/defer5.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testgo/defer5.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testgo/defer5.init$guard", align 1
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/defer5.main"(){{.*}} {
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
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/defer5.main", %_llgo_2), ptr %6, align 8
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
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_5
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/defer5.main", %_llgo_8), ptr %8, align 8
// CHECK-NEXT:   %13 = load i64, ptr %7, align 8
// CHECK-NEXT:   %14 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %15 = icmp ne ptr %14, null
// CHECK-NEXT:   br i1 %15, label %_llgo_9, label %_llgo_10
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_5, %_llgo_12
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Rethrow"(ptr %0)
// CHECK-NEXT:   br label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_4:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %16 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %17 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 32)
// CHECK-NEXT:   %18 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" }, ptr %17, i32 0, i32 0
// CHECK-NEXT:   store ptr %16, ptr %18, align 8
// CHECK-NEXT:   %19 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" }, ptr %17, i32 0, i32 1
// CHECK-NEXT:   store i64 0, ptr %19, align 8
// CHECK-NEXT:   %20 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" }, ptr %17, i32 0, i32 2
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @0, i64 1 }, ptr %20, align 8
// CHECK-NEXT:   store ptr %17, ptr %10, align 8
// CHECK-NEXT:   %21 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %22 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 32)
// CHECK-NEXT:   %23 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" }, ptr %22, i32 0, i32 0
// CHECK-NEXT:   store ptr %21, ptr %23, align 8
// CHECK-NEXT:   %24 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" }, ptr %22, i32 0, i32 1
// CHECK-NEXT:   store i64 3, ptr %24, align 8
// CHECK-NEXT:   %25 = getelementptr inbounds { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" }, ptr %22, i32 0, i32 2
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @1, i64 1 }, ptr %25, align 8
// CHECK-NEXT:   store ptr %22, ptr %10, align 8
// CHECK-NEXT:   %26 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @2, i64 13 }, ptr %26, align 8
// CHECK-NEXT:   %27 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %26, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %27)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_5:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/defer5.main", %_llgo_3), ptr %9, align 8
// CHECK-NEXT:   %28 = load ptr, ptr %8, align 8
// CHECK-NEXT:   indirectbr ptr %28, [label %_llgo_3, label %_llgo_6, label %_llgo_7, label %_llgo_8, label %_llgo_2]
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_6:                                          ; preds = %_llgo_5, %_llgo_7
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/defer5.main", %_llgo_3), ptr %8, align 8
// CHECK-NEXT:   %29 = load i64, ptr %7, align 8
// CHECK-NEXT:   %30 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %31 = icmp ne ptr %30, null
// CHECK-NEXT:   br i1 %31, label %_llgo_11, label %_llgo_12
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_7:                                          ; preds = %_llgo_5, %_llgo_8
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/defer5.main", %_llgo_6), ptr %8, align 8
// CHECK-NEXT:   %32 = load i64, ptr %7, align 8
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/defer5.main$1"()
// CHECK-NEXT:   br label %_llgo_6
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_8:                                          ; preds = %_llgo_5, %_llgo_10
// CHECK-NEXT:   store ptr blockaddress(@"{{.*}}/cl/_testgo/defer5.main", %_llgo_7), ptr %8, align 8
// CHECK-NEXT:   %33 = load i64, ptr %7, align 8
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/defer5.main$2"()
// CHECK-NEXT:   br label %_llgo_7
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_9:                                          ; preds = %_llgo_2
// CHECK-NEXT:   %34 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %35 = load { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" }, ptr %34, align 8
// CHECK-NEXT:   %36 = extractvalue { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" } %35, 0
// CHECK-NEXT:   store ptr %36, ptr %10, align 8
// CHECK-NEXT:   %37 = extractvalue { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" } %35, 2
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.FreeDeferNode"(ptr %34)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" %37)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_10
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_10:                                         ; preds = %_llgo_9, %_llgo_2
// CHECK-NEXT:   br label %_llgo_8
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_11:                                         ; preds = %_llgo_6
// CHECK-NEXT:   %38 = load ptr, ptr %10, align 8
// CHECK-NEXT:   %39 = load { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" }, ptr %38, align 8
// CHECK-NEXT:   %40 = extractvalue { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" } %39, 0
// CHECK-NEXT:   store ptr %40, ptr %10, align 8
// CHECK-NEXT:   %41 = extractvalue { ptr, i64, %"{{.*}}/runtime/internal/runtime.String" } %39, 2
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.FreeDeferNode"(ptr %38)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" %41)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_12
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_12:                                         ; preds = %_llgo_11, %_llgo_6
// CHECK-NEXT:   %42 = load %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, align 8
// CHECK-NEXT:   %43 = extractvalue %"{{.*}}/runtime/internal/runtime.Defer" %42, 2
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.SetThreadDefer"(ptr %43)
// CHECK-NEXT:   %44 = load ptr, ptr %9, align 8
// CHECK-NEXT:   indirectbr ptr %44, [label %_llgo_3]
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/defer5.main$1"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call %"{{.*}}/runtime/internal/runtime.eface" @"{{.*}}/runtime/internal/runtime.Recover"()
// CHECK-NEXT:   %1 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" %0, %"{{.*}}/runtime/internal/runtime.eface" zeroinitializer)
// CHECK-NEXT:   %2 = xor i1 %1, true
// CHECK-NEXT:   br i1 %2, label %_llgo_1, label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" { ptr @4, i64 10 })
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   %3 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @5, i64 16 }, ptr %3, align 8
// CHECK-NEXT:   %4 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %3, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %4)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/defer5.main$2"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" { ptr @6, i64 10 })
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @7, i64 16 }, ptr %0, align 8
// CHECK-NEXT:   %1 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %0, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %1)
// CHECK-NEXT:   unreachable
// CHECK-NEXT: }
