; ModuleID = 'main'
source_filename = "main"

%"github.com/goplus/llgo/internal/runtime.eface" = type { ptr, ptr }
%"github.com/goplus/llgo/internal/abi.Type" = type { i64, i64, i32, i8, i8, i8, i8, { ptr, ptr }, ptr, %"github.com/goplus/llgo/internal/runtime.String", ptr }
%"github.com/goplus/llgo/internal/runtime.String" = type { ptr, i64 }
%"github.com/goplus/llgo/internal/abi.StructField" = type { %"github.com/goplus/llgo/internal/runtime.String", ptr, i64, %"github.com/goplus/llgo/internal/runtime.String", i1 }
%"github.com/goplus/llgo/internal/runtime.Slice" = type { ptr, i64, i64 }

@"main.init$guard" = global ptr null
@"main.struct$MYpsoM99ZwFY087IpUOkIw1zjBA_sgFXVodmn1m-G88" = global ptr null
@_llgo_int = linkonce global ptr null
@0 = private unnamed_addr constant [2 x i8] c"v\00", align 1
@1 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@2 = private unnamed_addr constant [5 x i8] c"main\00", align 1
@__llgo_argc = global ptr null
@__llgo_argv = global ptr null
@3 = private unnamed_addr constant [12 x i8] c"Foo: not ok\00", align 1
@"_llgo_struct$K-dZ9QotZfVPz2a0YdRa9vmZUuDXPTqZOlMShKEDJtk" = linkonce global ptr null
@4 = private unnamed_addr constant [2 x i8] c"V\00", align 1
@5 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@6 = private unnamed_addr constant [5 x i8] c"main\00", align 1
@7 = private unnamed_addr constant [12 x i8] c"Bar: not ok\00", align 1
@8 = private unnamed_addr constant [10 x i8] c"F: not ok\00", align 1

define %"github.com/goplus/llgo/internal/runtime.eface" @main.Foo() {
_llgo_0:
  %0 = alloca { i64 }, align 8
  %1 = call ptr @"github.com/goplus/llgo/internal/runtime.Zeroinit"(ptr %0, i64 8)
  %2 = getelementptr inbounds { i64 }, ptr %1, i32 0, i32 0
  store i64 1, ptr %2, align 4
  %3 = load { i64 }, ptr %1, align 4
  %4 = load ptr, ptr @"main.struct$MYpsoM99ZwFY087IpUOkIw1zjBA_sgFXVodmn1m-G88", align 8
  %5 = extractvalue { i64 } %3, 0
  %6 = getelementptr inbounds %"github.com/goplus/llgo/internal/abi.Type", ptr %4, i32 0, i32 6
  %7 = load i8, ptr %6, align 1
  %8 = or i8 %7, 32
  store i8 %8, ptr %6, align 1
  %9 = inttoptr i64 %5 to ptr
  %10 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %11 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %10, i32 0, i32 0
  store ptr %4, ptr %11, align 8
  %12 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %10, i32 0, i32 1
  store ptr %9, ptr %12, align 8
  %13 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %10, align 8
  ret %"github.com/goplus/llgo/internal/runtime.eface" %13
}

define void @main.init() {
_llgo_0:
  %0 = load i1, ptr @"main.init$guard", align 1
  br i1 %0, label %_llgo_2, label %_llgo_1

_llgo_1:                                          ; preds = %_llgo_0
  store i1 true, ptr @"main.init$guard", align 1
  call void @"github.com/goplus/llgo/cl/internal/foo.init"()
  call void @"main.init$after"()
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
  ret void
}

define i32 @main(i32 %0, ptr %1) {
_llgo_0:
  store i32 %0, ptr @__llgo_argc, align 4
  store ptr %1, ptr @__llgo_argv, align 8
  call void @"github.com/goplus/llgo/internal/runtime.init"()
  call void @main.init()
  %2 = call %"github.com/goplus/llgo/internal/runtime.eface" @main.Foo()
  %3 = alloca { i64 }, align 8
  %4 = call ptr @"github.com/goplus/llgo/internal/runtime.Zeroinit"(ptr %3, i64 8)
  %5 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %2, 0
  %6 = load ptr, ptr @"main.struct$MYpsoM99ZwFY087IpUOkIw1zjBA_sgFXVodmn1m-G88", align 8
  %7 = icmp eq ptr %5, %6
  br i1 %7, label %_llgo_10, label %_llgo_11

_llgo_1:                                          ; preds = %_llgo_12
  %8 = getelementptr inbounds { i64 }, ptr %4, i32 0, i32 0
  %9 = load i64, ptr %8, align 4
  call void @"github.com/goplus/llgo/internal/runtime.PrintInt"(i64 %9)
  call void @"github.com/goplus/llgo/internal/runtime.PrintByte"(i8 10)
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_3, %_llgo_1
  %10 = call %"github.com/goplus/llgo/internal/runtime.eface" @"github.com/goplus/llgo/cl/internal/foo.Bar"()
  %11 = alloca { i64 }, align 8
  %12 = call ptr @"github.com/goplus/llgo/internal/runtime.Zeroinit"(ptr %11, i64 8)
  %13 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %10, 0
  %14 = load ptr, ptr @"_llgo_struct$K-dZ9QotZfVPz2a0YdRa9vmZUuDXPTqZOlMShKEDJtk", align 8
  %15 = icmp eq ptr %13, %14
  br i1 %15, label %_llgo_13, label %_llgo_14

_llgo_3:                                          ; preds = %_llgo_12
  %16 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %17 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %16, i32 0, i32 0
  store ptr @3, ptr %17, align 8
  %18 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %16, i32 0, i32 1
  store i64 11, ptr %18, align 4
  %19 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %16, align 8
  call void @"github.com/goplus/llgo/internal/runtime.PrintString"(%"github.com/goplus/llgo/internal/runtime.String" %19)
  call void @"github.com/goplus/llgo/internal/runtime.PrintByte"(i8 10)
  br label %_llgo_2

_llgo_4:                                          ; preds = %_llgo_15
  %20 = getelementptr inbounds { i64 }, ptr %12, i32 0, i32 0
  %21 = load i64, ptr %20, align 4
  call void @"github.com/goplus/llgo/internal/runtime.PrintInt"(i64 %21)
  call void @"github.com/goplus/llgo/internal/runtime.PrintByte"(i8 10)
  br label %_llgo_5

_llgo_5:                                          ; preds = %_llgo_6, %_llgo_4
  %22 = alloca { i64 }, align 8
  %23 = call ptr @"github.com/goplus/llgo/internal/runtime.Zeroinit"(ptr %22, i64 8)
  %24 = call %"github.com/goplus/llgo/internal/runtime.eface" @"github.com/goplus/llgo/cl/internal/foo.F"()
  %25 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %24, 0
  %26 = load ptr, ptr @"main.struct$MYpsoM99ZwFY087IpUOkIw1zjBA_sgFXVodmn1m-G88", align 8
  %27 = icmp eq ptr %25, %26
  br i1 %27, label %_llgo_16, label %_llgo_17

_llgo_6:                                          ; preds = %_llgo_15
  %28 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %29 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %28, i32 0, i32 0
  store ptr @7, ptr %29, align 8
  %30 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %28, i32 0, i32 1
  store i64 11, ptr %30, align 4
  %31 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %28, align 8
  call void @"github.com/goplus/llgo/internal/runtime.PrintString"(%"github.com/goplus/llgo/internal/runtime.String" %31)
  call void @"github.com/goplus/llgo/internal/runtime.PrintByte"(i8 10)
  br label %_llgo_5

_llgo_7:                                          ; preds = %_llgo_18
  %32 = getelementptr inbounds { i64 }, ptr %23, i32 0, i32 0
  %33 = load i64, ptr %32, align 4
  call void @"github.com/goplus/llgo/internal/runtime.PrintInt"(i64 %33)
  call void @"github.com/goplus/llgo/internal/runtime.PrintByte"(i8 10)
  br label %_llgo_8

_llgo_8:                                          ; preds = %_llgo_9, %_llgo_7
  ret i32 0

_llgo_9:                                          ; preds = %_llgo_18
  %34 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %35 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %34, i32 0, i32 0
  store ptr @8, ptr %35, align 8
  %36 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %34, i32 0, i32 1
  store i64 9, ptr %36, align 4
  %37 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %34, align 8
  call void @"github.com/goplus/llgo/internal/runtime.PrintString"(%"github.com/goplus/llgo/internal/runtime.String" %37)
  call void @"github.com/goplus/llgo/internal/runtime.PrintByte"(i8 10)
  br label %_llgo_8

_llgo_10:                                         ; preds = %_llgo_0
  %38 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %2, 1
  %39 = ptrtoint ptr %38 to i64
  %40 = alloca { i64 }, align 8
  %41 = getelementptr inbounds { i64 }, ptr %40, i32 0, i32 0
  store i64 %39, ptr %41, align 4
  %42 = load { i64 }, ptr %40, align 4
  %43 = alloca { { i64 }, i1 }, align 8
  %44 = getelementptr inbounds { { i64 }, i1 }, ptr %43, i32 0, i32 0
  store { i64 } %42, ptr %44, align 4
  %45 = getelementptr inbounds { { i64 }, i1 }, ptr %43, i32 0, i32 1
  store i1 true, ptr %45, align 1
  %46 = load { { i64 }, i1 }, ptr %43, align 4
  br label %_llgo_12

_llgo_11:                                         ; preds = %_llgo_0
  %47 = alloca { { i64 }, i1 }, align 8
  %48 = getelementptr inbounds { { i64 }, i1 }, ptr %47, i32 0, i32 0
  store { i64 } zeroinitializer, ptr %48, align 4
  %49 = getelementptr inbounds { { i64 }, i1 }, ptr %47, i32 0, i32 1
  store i1 false, ptr %49, align 1
  %50 = load { { i64 }, i1 }, ptr %47, align 4
  br label %_llgo_12

_llgo_12:                                         ; preds = %_llgo_11, %_llgo_10
  %51 = phi { { i64 }, i1 } [ %46, %_llgo_10 ], [ %50, %_llgo_11 ]
  %52 = extractvalue { { i64 }, i1 } %51, 0
  store { i64 } %52, ptr %4, align 4
  %53 = extractvalue { { i64 }, i1 } %51, 1
  br i1 %53, label %_llgo_1, label %_llgo_3

_llgo_13:                                         ; preds = %_llgo_2
  %54 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %10, 1
  %55 = ptrtoint ptr %54 to i64
  %56 = alloca { i64 }, align 8
  %57 = getelementptr inbounds { i64 }, ptr %56, i32 0, i32 0
  store i64 %55, ptr %57, align 4
  %58 = load { i64 }, ptr %56, align 4
  %59 = alloca { { i64 }, i1 }, align 8
  %60 = getelementptr inbounds { { i64 }, i1 }, ptr %59, i32 0, i32 0
  store { i64 } %58, ptr %60, align 4
  %61 = getelementptr inbounds { { i64 }, i1 }, ptr %59, i32 0, i32 1
  store i1 true, ptr %61, align 1
  %62 = load { { i64 }, i1 }, ptr %59, align 4
  br label %_llgo_15

_llgo_14:                                         ; preds = %_llgo_2
  %63 = alloca { { i64 }, i1 }, align 8
  %64 = getelementptr inbounds { { i64 }, i1 }, ptr %63, i32 0, i32 0
  store { i64 } zeroinitializer, ptr %64, align 4
  %65 = getelementptr inbounds { { i64 }, i1 }, ptr %63, i32 0, i32 1
  store i1 false, ptr %65, align 1
  %66 = load { { i64 }, i1 }, ptr %63, align 4
  br label %_llgo_15

_llgo_15:                                         ; preds = %_llgo_14, %_llgo_13
  %67 = phi { { i64 }, i1 } [ %62, %_llgo_13 ], [ %66, %_llgo_14 ]
  %68 = extractvalue { { i64 }, i1 } %67, 0
  store { i64 } %68, ptr %12, align 4
  %69 = extractvalue { { i64 }, i1 } %67, 1
  br i1 %69, label %_llgo_4, label %_llgo_6

_llgo_16:                                         ; preds = %_llgo_5
  %70 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %24, 1
  %71 = ptrtoint ptr %70 to i64
  %72 = alloca { i64 }, align 8
  %73 = getelementptr inbounds { i64 }, ptr %72, i32 0, i32 0
  store i64 %71, ptr %73, align 4
  %74 = load { i64 }, ptr %72, align 4
  %75 = alloca { { i64 }, i1 }, align 8
  %76 = getelementptr inbounds { { i64 }, i1 }, ptr %75, i32 0, i32 0
  store { i64 } %74, ptr %76, align 4
  %77 = getelementptr inbounds { { i64 }, i1 }, ptr %75, i32 0, i32 1
  store i1 true, ptr %77, align 1
  %78 = load { { i64 }, i1 }, ptr %75, align 4
  br label %_llgo_18

_llgo_17:                                         ; preds = %_llgo_5
  %79 = alloca { { i64 }, i1 }, align 8
  %80 = getelementptr inbounds { { i64 }, i1 }, ptr %79, i32 0, i32 0
  store { i64 } zeroinitializer, ptr %80, align 4
  %81 = getelementptr inbounds { { i64 }, i1 }, ptr %79, i32 0, i32 1
  store i1 false, ptr %81, align 1
  %82 = load { { i64 }, i1 }, ptr %79, align 4
  br label %_llgo_18

_llgo_18:                                         ; preds = %_llgo_17, %_llgo_16
  %83 = phi { { i64 }, i1 } [ %78, %_llgo_16 ], [ %82, %_llgo_17 ]
  %84 = extractvalue { { i64 }, i1 } %83, 0
  store { i64 } %84, ptr %23, align 4
  %85 = extractvalue { { i64 }, i1 } %83, 1
  br i1 %85, label %_llgo_7, label %_llgo_9
}

declare ptr @"github.com/goplus/llgo/internal/runtime.Zeroinit"(ptr, i64)

define void @"main.init$after"() {
_llgo_0:
  %0 = load ptr, ptr @_llgo_int, align 8
  %1 = icmp eq ptr %0, null
  br i1 %1, label %_llgo_1, label %_llgo_2

_llgo_1:                                          ; preds = %_llgo_0
  %2 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 2)
  store ptr %2, ptr @_llgo_int, align 8
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
  %3 = load ptr, ptr @_llgo_int, align 8
  %4 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %5 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %4, i32 0, i32 0
  store ptr @0, ptr %5, align 8
  %6 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %4, i32 0, i32 1
  store i64 1, ptr %6, align 4
  %7 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %4, align 8
  %8 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %9 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %8, i32 0, i32 0
  store ptr @1, ptr %9, align 8
  %10 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %8, i32 0, i32 1
  store i64 0, ptr %10, align 4
  %11 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %8, align 8
  %12 = call %"github.com/goplus/llgo/internal/abi.StructField" @"github.com/goplus/llgo/internal/runtime.StructField"(%"github.com/goplus/llgo/internal/runtime.String" %7, ptr %3, i64 0, %"github.com/goplus/llgo/internal/runtime.String" %11, i1 false)
  %13 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %14 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %13, i32 0, i32 0
  store ptr @2, ptr %14, align 8
  %15 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %13, i32 0, i32 1
  store i64 4, ptr %15, align 4
  %16 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %13, align 8
  %17 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocU"(i64 56)
  %18 = getelementptr %"github.com/goplus/llgo/internal/abi.StructField", ptr %17, i64 0
  store %"github.com/goplus/llgo/internal/abi.StructField" %12, ptr %18, align 8
  %19 = alloca %"github.com/goplus/llgo/internal/runtime.Slice", align 8
  %20 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.Slice", ptr %19, i32 0, i32 0
  store ptr %17, ptr %20, align 8
  %21 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.Slice", ptr %19, i32 0, i32 1
  store i64 1, ptr %21, align 4
  %22 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.Slice", ptr %19, i32 0, i32 2
  store i64 1, ptr %22, align 4
  %23 = load %"github.com/goplus/llgo/internal/runtime.Slice", ptr %19, align 8
  %24 = call ptr @"github.com/goplus/llgo/internal/runtime.Struct"(%"github.com/goplus/llgo/internal/runtime.String" %16, i64 8, %"github.com/goplus/llgo/internal/runtime.Slice" %23)
  store ptr %24, ptr @"main.struct$MYpsoM99ZwFY087IpUOkIw1zjBA_sgFXVodmn1m-G88", align 8
  %25 = load ptr, ptr @_llgo_int, align 8
  %26 = load ptr, ptr @"_llgo_struct$K-dZ9QotZfVPz2a0YdRa9vmZUuDXPTqZOlMShKEDJtk", align 8
  %27 = icmp eq ptr %26, null
  br i1 %27, label %_llgo_3, label %_llgo_4

_llgo_3:                                          ; preds = %_llgo_2
  %28 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %29 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %28, i32 0, i32 0
  store ptr @4, ptr %29, align 8
  %30 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %28, i32 0, i32 1
  store i64 1, ptr %30, align 4
  %31 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %28, align 8
  %32 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %33 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %32, i32 0, i32 0
  store ptr @5, ptr %33, align 8
  %34 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %32, i32 0, i32 1
  store i64 0, ptr %34, align 4
  %35 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %32, align 8
  %36 = call %"github.com/goplus/llgo/internal/abi.StructField" @"github.com/goplus/llgo/internal/runtime.StructField"(%"github.com/goplus/llgo/internal/runtime.String" %31, ptr %25, i64 0, %"github.com/goplus/llgo/internal/runtime.String" %35, i1 false)
  %37 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %38 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %37, i32 0, i32 0
  store ptr @6, ptr %38, align 8
  %39 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %37, i32 0, i32 1
  store i64 4, ptr %39, align 4
  %40 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %37, align 8
  %41 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocU"(i64 56)
  %42 = getelementptr %"github.com/goplus/llgo/internal/abi.StructField", ptr %41, i64 0
  store %"github.com/goplus/llgo/internal/abi.StructField" %36, ptr %42, align 8
  %43 = alloca %"github.com/goplus/llgo/internal/runtime.Slice", align 8
  %44 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.Slice", ptr %43, i32 0, i32 0
  store ptr %41, ptr %44, align 8
  %45 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.Slice", ptr %43, i32 0, i32 1
  store i64 1, ptr %45, align 4
  %46 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.Slice", ptr %43, i32 0, i32 2
  store i64 1, ptr %46, align 4
  %47 = load %"github.com/goplus/llgo/internal/runtime.Slice", ptr %43, align 8
  %48 = call ptr @"github.com/goplus/llgo/internal/runtime.Struct"(%"github.com/goplus/llgo/internal/runtime.String" %40, i64 8, %"github.com/goplus/llgo/internal/runtime.Slice" %47)
  store ptr %48, ptr @"_llgo_struct$K-dZ9QotZfVPz2a0YdRa9vmZUuDXPTqZOlMShKEDJtk", align 8
  br label %_llgo_4

_llgo_4:                                          ; preds = %_llgo_3, %_llgo_2
  ret void
}

declare ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64)

declare ptr @"github.com/goplus/llgo/internal/runtime.Struct"(%"github.com/goplus/llgo/internal/runtime.String", i64, %"github.com/goplus/llgo/internal/runtime.Slice")

declare %"github.com/goplus/llgo/internal/abi.StructField" @"github.com/goplus/llgo/internal/runtime.StructField"(%"github.com/goplus/llgo/internal/runtime.String", ptr, i64, %"github.com/goplus/llgo/internal/runtime.String", i1)

declare ptr @"github.com/goplus/llgo/internal/runtime.AllocU"(i64)

declare void @"github.com/goplus/llgo/cl/internal/foo.init"()

declare void @"github.com/goplus/llgo/internal/runtime.init"()

declare void @"github.com/goplus/llgo/internal/runtime.PrintInt"(i64)

declare void @"github.com/goplus/llgo/internal/runtime.PrintByte"(i8)

declare void @"github.com/goplus/llgo/internal/runtime.PrintString"(%"github.com/goplus/llgo/internal/runtime.String")

declare %"github.com/goplus/llgo/internal/runtime.eface" @"github.com/goplus/llgo/cl/internal/foo.Bar"()

declare %"github.com/goplus/llgo/internal/runtime.eface" @"github.com/goplus/llgo/cl/internal/foo.F"()
