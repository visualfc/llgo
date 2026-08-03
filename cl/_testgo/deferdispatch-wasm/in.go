// LITTEST
package main

// CHECK: {{^}}@0 = private unnamed_addr constant [5 x i8] c"first", align 1{{$}}
// CHECK: {{^}}@1 = private unnamed_addr constant [6 x i8] c"second", align 1{{$}}

func run() {
	defer println("first")
	defer println("second")
}

func main() {
	run()
}

// CHECK-LABEL: define void @main.init(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"main.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"main.init$guard", align 1
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @main.main(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   call void @main.run()
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @main.run(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.GetThreadDefer"()
// CHECK-NEXT:   %1 = alloca i8, i32 200, align 1
// CHECK-NEXT:   %2 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i32 24)
// CHECK-NEXT:   %3 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 0
// CHECK-NEXT:   store ptr %1, ptr %3, align 4
// CHECK-NEXT:   %4 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 1
// CHECK-NEXT:   store i32 0, ptr %4, align 4
// CHECK-NEXT:   %5 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 2
// CHECK-NEXT:   store ptr %0, ptr %5, align 4
// CHECK-NEXT:   %6 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 3
// CHECK-NEXT:   store ptr null, ptr %6, align 4
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.SetThreadDefer"(ptr %2)
// CHECK-NEXT:   %7 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 1
// CHECK-NEXT:   %8 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 3
// CHECK-NEXT:   %9 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 4
// CHECK-NEXT:   %10 = getelementptr inbounds %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, i32 0, i32 5
// CHECK-NEXT:   store ptr null, ptr %9, align 4
// CHECK-NEXT:   store ptr null, ptr %10, align 4
// CHECK-NEXT:   %11 = call i32 @setjmp(ptr %1)
// CHECK-NEXT:   %12 = icmp eq i32 %11, 0
// CHECK-NEXT:   br i1 %12, label %_llgo_4, label %_llgo_5
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_3
// CHECK-NEXT:   ret void
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_5, %_llgo_4
// CHECK-NEXT:   store ptr inttoptr (i32 2 to ptr), ptr %8, align 4
// CHECK-NEXT:   %13 = load i32, ptr %7, align 4
// CHECK-NEXT:   %14 = load ptr, ptr %10, align 4
// CHECK-NEXT:   %15 = icmp ne ptr %14, null
// CHECK-NEXT:   br i1 %15, label %_llgo_8, label %_llgo_9
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_5, %_llgo_11
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Rethrow"(ptr %0)
// CHECK-NEXT:   br label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_4:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %16 = load ptr, ptr %10, align 4
// CHECK-NEXT:   %17 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i32 16)
// CHECK-NEXT:   %18 = getelementptr inbounds { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" }, ptr %17, i32 0, i32 0
// CHECK-NEXT:   store ptr %16, ptr %18, align 4
// CHECK-NEXT:   %19 = getelementptr inbounds { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" }, ptr %17, i32 0, i32 1
// CHECK-NEXT:   store i32 0, ptr %19, align 4
// CHECK-NEXT:   %20 = getelementptr inbounds { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" }, ptr %17, i32 0, i32 2
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @0, i32 5 }, ptr %20, align 4
// CHECK-NEXT:   store ptr %17, ptr %10, align 4
// CHECK-NEXT:   %21 = load ptr, ptr %10, align 4
// CHECK-NEXT:   %22 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i32 16)
// CHECK-NEXT:   %23 = getelementptr inbounds { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" }, ptr %22, i32 0, i32 0
// CHECK-NEXT:   store ptr %21, ptr %23, align 4
// CHECK-NEXT:   %24 = getelementptr inbounds { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" }, ptr %22, i32 0, i32 1
// CHECK-NEXT:   store i32 1, ptr %24, align 4
// CHECK-NEXT:   %25 = getelementptr inbounds { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" }, ptr %22, i32 0, i32 2
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @1, i32 6 }, ptr %25, align 4
// CHECK-NEXT:   store ptr %22, ptr %10, align 4
// CHECK-NEXT:   store ptr inttoptr (i32 1 to ptr), ptr %9, align 4
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_5:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store ptr null, ptr %9, align 4
// CHECK-NEXT:   %26 = load ptr, ptr %8, align 4
// CHECK-NEXT:   %27 = ptrtoint ptr %26 to i32
// CHECK-NEXT:   switch i32 %27, label %_llgo_13 [
// CHECK-NEXT:     i32 1, label %_llgo_3
// CHECK-NEXT:     i32 2, label %_llgo_7
// CHECK-NEXT:     i32 0, label %_llgo_2
// CHECK-NEXT:   ]
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_6:                                          ; preds = %_llgo_11
// CHECK-NEXT:   ret void
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_7:                                          ; preds = %_llgo_5, %_llgo_9
// CHECK-NEXT:   store ptr inttoptr (i32 1 to ptr), ptr %8, align 4
// CHECK-NEXT:   %28 = load i32, ptr %7, align 4
// CHECK-NEXT:   %29 = load ptr, ptr %10, align 4
// CHECK-NEXT:   %30 = icmp ne ptr %29, null
// CHECK-NEXT:   br i1 %30, label %_llgo_10, label %_llgo_11
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_8:                                          ; preds = %_llgo_2
// CHECK-NEXT:   %31 = load ptr, ptr %10, align 4
// CHECK-NEXT:   %32 = load { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" }, ptr %31, align 4
// CHECK-NEXT:   %33 = extractvalue { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" } %32, 0
// CHECK-NEXT:   store ptr %33, ptr %10, align 4
// CHECK-NEXT:   %34 = extractvalue { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" } %32, 2
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.FreeDeferNode"(ptr %31)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" %34)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_9
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_9:                                          ; preds = %_llgo_8, %_llgo_2
// CHECK-NEXT:   br label %_llgo_7
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_10:                                         ; preds = %_llgo_7
// CHECK-NEXT:   %35 = load ptr, ptr %10, align 4
// CHECK-NEXT:   %36 = load { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" }, ptr %35, align 4
// CHECK-NEXT:   %37 = extractvalue { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" } %36, 0
// CHECK-NEXT:   store ptr %37, ptr %10, align 4
// CHECK-NEXT:   %38 = extractvalue { ptr, i32, %"{{.*}}/runtime/internal/runtime.String" } %36, 2
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.FreeDeferNode"(ptr %35)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" %38)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_11
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_11:                                         ; preds = %_llgo_10, %_llgo_7
// CHECK-NEXT:   %39 = load %"{{.*}}/runtime/internal/runtime.Defer", ptr %2, align 4
// CHECK-NEXT:   %40 = extractvalue %"{{.*}}/runtime/internal/runtime.Defer" %39, 2
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.SetThreadDefer"(ptr %40)
// CHECK-NEXT:   %41 = load ptr, ptr %9, align 4
// CHECK-NEXT:   %42 = ptrtoint ptr %41 to i32
// CHECK-NEXT:   switch i32 %42, label %_llgo_12 [
// CHECK-NEXT:     i32 0, label %_llgo_3
// CHECK-NEXT:     i32 1, label %_llgo_6
// CHECK-NEXT:   ]
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_12:                                         ; preds = %_llgo_11
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_13:                                         ; preds = %_llgo_5
// CHECK-NEXT:   unreachable
// CHECK-NEXT: }
