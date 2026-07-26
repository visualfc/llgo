// LITTEST
package main

import "github.com/goplus/llgo/cl/_testdata/foo"

// CHECK: {{^}}@4 = private unnamed_addr constant [11 x i8] c"Foo: not ok", align 1{{$}}
// CHECK: {{^}}@7 = private unnamed_addr constant [11 x i8] c"Bar: not ok", align 1{{$}}
// CHECK: {{^}}@8 = private unnamed_addr constant [9 x i8] c"F: not ok", align 1{{$}}

// CHECK-LABEL: define %"{{.*}}/runtime/internal/runtime.eface" @"{{.*}}/cl/_testgo/strucintf.Foo"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = alloca { i64 }, align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %0, i8 0, i64 8, i1 false)
// CHECK-NEXT:   %1 = getelementptr inbounds { i64 }, ptr %0, i32 0, i32 0
// CHECK-NEXT:   store i64 1, ptr %1, align 8
// CHECK-NEXT:   %2 = load { i64 }, ptr %0, align 8
// CHECK-NEXT:   %3 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 8)
// CHECK-NEXT:   store { i64 } %2, ptr %3, align 8
// CHECK-NEXT:   %4 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"{{.*}}/cl/_testgo/strucintf.struct$MYpsoM99ZwFY087IpUOkIw1zjBA_sgFXVodmn1m-G88", ptr undef }, ptr %3, 1
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.eface" %4
// CHECK-NEXT: }

func Foo() any {
	return struct{ v int }{1}
}

func main() {
	v := Foo()

	if x, ok := v.(struct{ v int }); ok {
		println(x.v)
	} else {
		println("Foo: not ok")
	}

	bar := foo.Bar()

	if x, ok := bar.(struct{ V int }); ok {
		println(x.V)
	} else {
		println("Bar: not ok")
	}

	if x, ok := foo.F().(struct{ v int }); ok {
		println(x.v)
	} else {
		println("F: not ok")
	}
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/strucintf.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testgo/strucintf.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testgo/strucintf.init$guard", align 1
// CHECK-NEXT:   call void @"{{.*}}/cl/_testdata/foo.init"()
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/strucintf.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call %"{{.*}}/runtime/internal/runtime.eface" @"{{.*}}/cl/_testgo/strucintf.Foo"()
// CHECK-NEXT:   %1 = alloca { i64 }, align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %1, i8 0, i64 8, i1 false)
// CHECK-NEXT:   %2 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %0, 0
// CHECK-NEXT:   %3 = icmp eq ptr %2, @"{{.*}}/cl/_testgo/strucintf.struct$MYpsoM99ZwFY087IpUOkIw1zjBA_sgFXVodmn1m-G88"
// CHECK-NEXT:   br i1 %3, label %_llgo_10, label %_llgo_11
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_12
// CHECK-NEXT:   %4 = getelementptr inbounds { i64 }, ptr %1, i32 0, i32 0
// CHECK-NEXT:   %5 = load i64, ptr %4, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintInt"(i64 %5)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_3, %_llgo_1
// CHECK-NEXT:   %6 = call %"{{.*}}/runtime/internal/runtime.eface" @"{{.*}}/cl/_testdata/foo.Bar"()
// CHECK-NEXT:   %7 = alloca { i64 }, align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %7, i8 0, i64 8, i1 false)
// CHECK-NEXT:   %8 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %6, 0
// CHECK-NEXT:   %9 = icmp eq ptr %8, @"_llgo_struct$K-dZ9QotZfVPz2a0YdRa9vmZUuDXPTqZOlMShKEDJtk"
// CHECK-NEXT:   br i1 %9, label %_llgo_13, label %_llgo_14
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_12
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" { ptr @4, i64 11 })
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_4:                                          ; preds = %_llgo_15
// CHECK-NEXT:   %10 = getelementptr inbounds { i64 }, ptr %7, i32 0, i32 0
// CHECK-NEXT:   %11 = load i64, ptr %10, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintInt"(i64 %11)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_5
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_5:                                          ; preds = %_llgo_6, %_llgo_4
// CHECK-NEXT:   %12 = alloca { i64 }, align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %12, i8 0, i64 8, i1 false)
// CHECK-NEXT:   %13 = call %"{{.*}}/runtime/internal/runtime.eface" @"{{.*}}/cl/_testdata/foo.F"()
// CHECK-NEXT:   %14 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %13, 0
// CHECK-NEXT:   %15 = icmp eq ptr %14, @"{{.*}}/cl/_testgo/strucintf.struct$MYpsoM99ZwFY087IpUOkIw1zjBA_sgFXVodmn1m-G88"
// CHECK-NEXT:   br i1 %15, label %_llgo_16, label %_llgo_17
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_6:                                          ; preds = %_llgo_15
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" { ptr @7, i64 11 })
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_5
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_7:                                          ; preds = %_llgo_18
// CHECK-NEXT:   %16 = getelementptr inbounds { i64 }, ptr %12, i32 0, i32 0
// CHECK-NEXT:   %17 = load i64, ptr %16, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintInt"(i64 %17)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_8
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_8:                                          ; preds = %_llgo_9, %_llgo_7
// CHECK-NEXT:   ret void
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_9:                                          ; preds = %_llgo_18
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" { ptr @8, i64 9 })
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_8
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_10:                                         ; preds = %_llgo_0
// CHECK-NEXT:   %18 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %0, 1
// CHECK-NEXT:   %19 = load { i64 }, ptr %18, align 8
// CHECK-NEXT:   %20 = insertvalue { { i64 }, i1 } undef, { i64 } %19, 0
// CHECK-NEXT:   %21 = insertvalue { { i64 }, i1 } %20, i1 true, 1
// CHECK-NEXT:   br label %_llgo_12
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_11:                                         ; preds = %_llgo_0
// CHECK-NEXT:   br label %_llgo_12
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_12:                                         ; preds = %_llgo_11, %_llgo_10
// CHECK-NEXT:   %22 = phi { { i64 }, i1 } [ %21, %_llgo_10 ], [ zeroinitializer, %_llgo_11 ]
// CHECK-NEXT:   %23 = extractvalue { { i64 }, i1 } %22, 0
// CHECK-NEXT:   store { i64 } %23, ptr %1, align 8
// CHECK-NEXT:   %24 = extractvalue { { i64 }, i1 } %22, 1
// CHECK-NEXT:   br i1 %24, label %_llgo_1, label %_llgo_3
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_13:                                         ; preds = %_llgo_2
// CHECK-NEXT:   %25 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %6, 1
// CHECK-NEXT:   %26 = load { i64 }, ptr %25, align 8
// CHECK-NEXT:   %27 = insertvalue { { i64 }, i1 } undef, { i64 } %26, 0
// CHECK-NEXT:   %28 = insertvalue { { i64 }, i1 } %27, i1 true, 1
// CHECK-NEXT:   br label %_llgo_15
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_14:                                         ; preds = %_llgo_2
// CHECK-NEXT:   br label %_llgo_15
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_15:                                         ; preds = %_llgo_14, %_llgo_13
// CHECK-NEXT:   %29 = phi { { i64 }, i1 } [ %28, %_llgo_13 ], [ zeroinitializer, %_llgo_14 ]
// CHECK-NEXT:   %30 = extractvalue { { i64 }, i1 } %29, 0
// CHECK-NEXT:   store { i64 } %30, ptr %7, align 8
// CHECK-NEXT:   %31 = extractvalue { { i64 }, i1 } %29, 1
// CHECK-NEXT:   br i1 %31, label %_llgo_4, label %_llgo_6
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_16:                                         ; preds = %_llgo_5
// CHECK-NEXT:   %32 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %13, 1
// CHECK-NEXT:   %33 = load { i64 }, ptr %32, align 8
// CHECK-NEXT:   %34 = insertvalue { { i64 }, i1 } undef, { i64 } %33, 0
// CHECK-NEXT:   %35 = insertvalue { { i64 }, i1 } %34, i1 true, 1
// CHECK-NEXT:   br label %_llgo_18
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_17:                                         ; preds = %_llgo_5
// CHECK-NEXT:   br label %_llgo_18
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_18:                                         ; preds = %_llgo_17, %_llgo_16
// CHECK-NEXT:   %36 = phi { { i64 }, i1 } [ %35, %_llgo_16 ], [ zeroinitializer, %_llgo_17 ]
// CHECK-NEXT:   %37 = extractvalue { { i64 }, i1 } %36, 0
// CHECK-NEXT:   store { i64 } %37, ptr %12, align 8
// CHECK-NEXT:   %38 = extractvalue { { i64 }, i1 } %36, 1
// CHECK-NEXT:   br i1 %38, label %_llgo_7, label %_llgo_9
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.memequal64"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.memequal64"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }
