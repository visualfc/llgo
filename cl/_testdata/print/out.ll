; ModuleID = 'main'
source_filename = "main"

%"github.com/goplus/llgo/internal/runtime.Slice" = type { ptr, i64, i64 }
%"github.com/goplus/llgo/internal/runtime.String" = type { ptr, i64 }
%main.stringStruct = type { ptr, i64 }
%main.slice = type { ptr, i64, i64 }
%"github.com/goplus/llgo/internal/runtime.eface" = type { ptr, ptr }

@"main.init$guard" = global ptr null
@main.minhexdigits = global ptr null
@0 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@__llgo_argc = global ptr null
@__llgo_argv = global ptr null
@1 = private unnamed_addr constant [5 x i8] c"llgo\00", align 1
@2 = private unnamed_addr constant [11 x i8] c"check bool\00", align 1
@3 = private unnamed_addr constant [9 x i8] c"check &^\00", align 1
@4 = private unnamed_addr constant [5 x i8] c"llgo\00", align 1
@5 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@6 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@7 = private unnamed_addr constant [4 x i8] c"NaN\00", align 1
@8 = private unnamed_addr constant [5 x i8] c"+Inf\00", align 1
@9 = private unnamed_addr constant [5 x i8] c"-Inf\00", align 1
@10 = private unnamed_addr constant [17 x i8] c"0123456789abcdef\00", align 1
@11 = private unnamed_addr constant [2 x i8] c"-\00", align 1
@12 = private unnamed_addr constant [2 x i8] c" \00", align 1
@13 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@14 = private unnamed_addr constant [2 x i8] c" \00", align 1

define %"github.com/goplus/llgo/internal/runtime.Slice" @main.bytes(%"github.com/goplus/llgo/internal/runtime.String" %0) {
_llgo_0:
  %1 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocZ"(i64 16)
  store %"github.com/goplus/llgo/internal/runtime.String" %0, ptr %1, align 8
  %2 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocZ"(i64 24)
  %3 = call ptr @main.stringStructOf(ptr %1)
  %4 = getelementptr inbounds %main.stringStruct, ptr %3, i32 0, i32 0
  %5 = load ptr, ptr %4, align 8
  %6 = getelementptr inbounds %main.slice, ptr %2, i32 0, i32 0
  store ptr %5, ptr %6, align 8
  %7 = getelementptr inbounds %main.stringStruct, ptr %3, i32 0, i32 1
  %8 = load i64, ptr %7, align 4
  %9 = getelementptr inbounds %main.slice, ptr %2, i32 0, i32 1
  store i64 %8, ptr %9, align 4
  %10 = getelementptr inbounds %main.stringStruct, ptr %3, i32 0, i32 1
  %11 = load i64, ptr %10, align 4
  %12 = getelementptr inbounds %main.slice, ptr %2, i32 0, i32 2
  store i64 %11, ptr %12, align 4
  %13 = load %"github.com/goplus/llgo/internal/runtime.Slice", ptr %2, align 8
  ret %"github.com/goplus/llgo/internal/runtime.Slice" %13
}

define void @main.gwrite(%"github.com/goplus/llgo/internal/runtime.Slice" %0) {
_llgo_0:
  %1 = extractvalue %"github.com/goplus/llgo/internal/runtime.Slice" %0, 1
  %2 = icmp eq i64 %1, 0
  br i1 %2, label %_llgo_1, label %_llgo_2

_llgo_1:                                          ; preds = %_llgo_0
  ret void

_llgo_2:                                          ; preds = %_llgo_0
  %3 = call i32 (ptr, ...) @printf(ptr @0, %"github.com/goplus/llgo/internal/runtime.Slice" %0)
  ret void
}

define void @main.init() {
_llgo_0:
  %0 = load i1, ptr @"main.init$guard", align 1
  br i1 %0, label %_llgo_2, label %_llgo_1

_llgo_1:                                          ; preds = %_llgo_0
  store i1 true, ptr @"main.init$guard", align 1
  store i64 0, ptr @main.minhexdigits, align 4
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
  %2 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %3 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %2, i32 0, i32 0
  store ptr @1, ptr %3, align 8
  %4 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %2, i32 0, i32 1
  store i64 4, ptr %4, align 4
  %5 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %2, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %5)
  call void @main.printnl()
  call void @main.printuint(i64 1024)
  call void @main.printnl()
  call void @main.printhex(i64 305441743)
  call void @main.printnl()
  call void @main.prinxor(i64 1)
  call void @main.printnl()
  call void @main.prinsub(i64 100)
  call void @main.printnl()
  call void @main.prinusub(i64 -1)
  call void @main.printnl()
  call void @main.prinfsub(double 1.001000e+02)
  call void @main.printnl()
  %6 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 13)
  %7 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %8 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %7, i32 0, i32 0
  store ptr %6, ptr %8, align 8
  %9 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %7, i32 0, i32 1
  store ptr inttoptr (i32 1315859240 to ptr), ptr %9, align 8
  %10 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %7, align 8
  call void @main.printany(%"github.com/goplus/llgo/internal/runtime.eface" %10)
  call void @main.printnl()
  %11 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 14)
  %12 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %13 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %12, i32 0, i32 0
  store ptr %11, ptr %13, align 8
  %14 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %12, i32 0, i32 1
  store ptr inttoptr (i64 4746175415993761792 to ptr), ptr %14, align 8
  %15 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %12, align 8
  call void @main.printany(%"github.com/goplus/llgo/internal/runtime.eface" %15)
  call void @main.printnl()
  br i1 true, label %_llgo_3, label %_llgo_2

_llgo_1:                                          ; preds = %_llgo_3
  %16 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocZ"(i64 32)
  %17 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %16, i64 0
  %18 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %19 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %18, i32 0, i32 0
  store ptr @2, ptr %19, align 8
  %20 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %18, i32 0, i32 1
  store i64 10, ptr %20, align 4
  %21 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %18, align 8
  %22 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 24)
  %23 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocU"(i64 16)
  store %"github.com/goplus/llgo/internal/runtime.String" %21, ptr %23, align 8
  %24 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %25 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %24, i32 0, i32 0
  store ptr %22, ptr %25, align 8
  %26 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %24, i32 0, i32 1
  store ptr %23, ptr %26, align 8
  %27 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %24, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %27, ptr %17, align 8
  %28 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %16, i64 1
  %29 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 1)
  %30 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %31 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %30, i32 0, i32 0
  store ptr %29, ptr %31, align 8
  %32 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %30, i32 0, i32 1
  store ptr inttoptr (i64 -1 to ptr), ptr %32, align 8
  %33 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %30, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %33, ptr %28, align 8
  %34 = call %"github.com/goplus/llgo/internal/runtime.Slice" @"github.com/goplus/llgo/internal/runtime.NewSlice3"(ptr %16, i64 16, i64 2, i64 0, i64 2, i64 2)
  call void @main.println(%"github.com/goplus/llgo/internal/runtime.Slice" %34)
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_3, %_llgo_1, %_llgo_0
  %35 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocZ"(i64 48)
  %36 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %35, i64 0
  %37 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %38 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %37, i32 0, i32 0
  store ptr @3, ptr %38, align 8
  %39 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %37, i32 0, i32 1
  store i64 8, ptr %39, align 4
  %40 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %37, align 8
  %41 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 24)
  %42 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocU"(i64 16)
  store %"github.com/goplus/llgo/internal/runtime.String" %40, ptr %42, align 8
  %43 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %44 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %43, i32 0, i32 0
  store ptr %41, ptr %44, align 8
  %45 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %43, i32 0, i32 1
  store ptr %42, ptr %45, align 8
  %46 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %43, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %46, ptr %36, align 8
  %47 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %35, i64 1
  %48 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 1)
  %49 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %50 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %49, i32 0, i32 0
  store ptr %48, ptr %50, align 8
  %51 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %49, i32 0, i32 1
  store ptr inttoptr (i64 -1 to ptr), ptr %51, align 8
  %52 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %49, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %52, ptr %47, align 8
  %53 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %35, i64 2
  %54 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 1)
  %55 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %56 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %55, i32 0, i32 0
  store ptr %54, ptr %56, align 8
  %57 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %55, i32 0, i32 1
  store ptr inttoptr (i64 -1 to ptr), ptr %57, align 8
  %58 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %55, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %58, ptr %53, align 8
  %59 = call %"github.com/goplus/llgo/internal/runtime.Slice" @"github.com/goplus/llgo/internal/runtime.NewSlice3"(ptr %35, i64 16, i64 3, i64 0, i64 3, i64 3)
  call void @main.println(%"github.com/goplus/llgo/internal/runtime.Slice" %59)
  %60 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocZ"(i64 256)
  %61 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 0
  %62 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 1)
  %63 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %64 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %63, i32 0, i32 0
  store ptr %62, ptr %64, align 8
  %65 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %63, i32 0, i32 1
  store ptr inttoptr (i64 -1 to ptr), ptr %65, align 8
  %66 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %63, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %66, ptr %61, align 8
  %67 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 1
  %68 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 1)
  %69 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %70 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %69, i32 0, i32 0
  store ptr %68, ptr %70, align 8
  %71 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %69, i32 0, i32 1
  store ptr null, ptr %71, align 8
  %72 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %69, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %72, ptr %67, align 8
  %73 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 2
  %74 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 5)
  %75 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %76 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %75, i32 0, i32 0
  store ptr %74, ptr %76, align 8
  %77 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %75, i32 0, i32 1
  store ptr inttoptr (i64 97 to ptr), ptr %77, align 8
  %78 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %75, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %78, ptr %73, align 8
  %79 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 3
  %80 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 5)
  %81 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %82 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %81, i32 0, i32 0
  store ptr %80, ptr %82, align 8
  %83 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %81, i32 0, i32 1
  store ptr inttoptr (i64 65 to ptr), ptr %83, align 8
  %84 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %81, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %84, ptr %79, align 8
  %85 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 4
  %86 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 5)
  %87 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %88 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %87, i32 0, i32 0
  store ptr %86, ptr %88, align 8
  %89 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %87, i32 0, i32 1
  store ptr inttoptr (i64 20013 to ptr), ptr %89, align 8
  %90 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %87, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %90, ptr %85, align 8
  %91 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 5
  %92 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 3)
  %93 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %94 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %93, i32 0, i32 0
  store ptr %92, ptr %94, align 8
  %95 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %93, i32 0, i32 1
  store ptr inttoptr (i64 1 to ptr), ptr %95, align 8
  %96 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %93, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %96, ptr %91, align 8
  %97 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 6
  %98 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 4)
  %99 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %100 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %99, i32 0, i32 0
  store ptr %98, ptr %100, align 8
  %101 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %99, i32 0, i32 1
  store ptr inttoptr (i64 2 to ptr), ptr %101, align 8
  %102 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %99, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %102, ptr %97, align 8
  %103 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 7
  %104 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 5)
  %105 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %106 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %105, i32 0, i32 0
  store ptr %104, ptr %106, align 8
  %107 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %105, i32 0, i32 1
  store ptr inttoptr (i64 3 to ptr), ptr %107, align 8
  %108 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %105, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %108, ptr %103, align 8
  %109 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 8
  %110 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 6)
  %111 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %112 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %111, i32 0, i32 0
  store ptr %110, ptr %112, align 8
  %113 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %111, i32 0, i32 1
  store ptr inttoptr (i64 4 to ptr), ptr %113, align 8
  %114 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %111, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %114, ptr %109, align 8
  %115 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 9
  %116 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 2)
  %117 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %118 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %117, i32 0, i32 0
  store ptr %116, ptr %118, align 8
  %119 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %117, i32 0, i32 1
  store ptr inttoptr (i64 5 to ptr), ptr %119, align 8
  %120 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %117, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %120, ptr %115, align 8
  %121 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 10
  %122 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 8)
  %123 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %124 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %123, i32 0, i32 0
  store ptr %122, ptr %124, align 8
  %125 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %123, i32 0, i32 1
  store ptr inttoptr (i64 1 to ptr), ptr %125, align 8
  %126 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %123, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %126, ptr %121, align 8
  %127 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 11
  %128 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 9)
  %129 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %130 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %129, i32 0, i32 0
  store ptr %128, ptr %130, align 8
  %131 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %129, i32 0, i32 1
  store ptr inttoptr (i64 2 to ptr), ptr %131, align 8
  %132 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %129, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %132, ptr %127, align 8
  %133 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 12
  %134 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 10)
  %135 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %136 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %135, i32 0, i32 0
  store ptr %134, ptr %136, align 8
  %137 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %135, i32 0, i32 1
  store ptr inttoptr (i64 3 to ptr), ptr %137, align 8
  %138 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %135, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %138, ptr %133, align 8
  %139 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 13
  %140 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 11)
  %141 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %142 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %141, i32 0, i32 0
  store ptr %140, ptr %142, align 8
  %143 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %141, i32 0, i32 1
  store ptr inttoptr (i64 4 to ptr), ptr %143, align 8
  %144 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %141, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %144, ptr %139, align 8
  %145 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 14
  %146 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 12)
  %147 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %148 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %147, i32 0, i32 0
  store ptr %146, ptr %148, align 8
  %149 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %147, i32 0, i32 1
  store ptr inttoptr (i64 5 to ptr), ptr %149, align 8
  %150 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %147, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %150, ptr %145, align 8
  %151 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %60, i64 15
  %152 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %153 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %152, i32 0, i32 0
  store ptr @4, ptr %153, align 8
  %154 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %152, i32 0, i32 1
  store i64 4, ptr %154, align 4
  %155 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %152, align 8
  %156 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 24)
  %157 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocU"(i64 16)
  store %"github.com/goplus/llgo/internal/runtime.String" %155, ptr %157, align 8
  %158 = alloca %"github.com/goplus/llgo/internal/runtime.eface", align 8
  %159 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %158, i32 0, i32 0
  store ptr %156, ptr %159, align 8
  %160 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %158, i32 0, i32 1
  store ptr %157, ptr %160, align 8
  %161 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %158, align 8
  store %"github.com/goplus/llgo/internal/runtime.eface" %161, ptr %151, align 8
  %162 = call %"github.com/goplus/llgo/internal/runtime.Slice" @"github.com/goplus/llgo/internal/runtime.NewSlice3"(ptr %60, i64 16, i64 16, i64 0, i64 16, i64 16)
  call void @main.println(%"github.com/goplus/llgo/internal/runtime.Slice" %162)
  ret i32 0

_llgo_3:                                          ; preds = %_llgo_0
  br i1 true, label %_llgo_1, label %_llgo_2
}

define void @main.prinfsub(double %0) {
_llgo_0:
  %1 = fneg double %0
  call void @main.printfloat(double %1)
  ret void
}

define void @main.prinsub(i64 %0) {
_llgo_0:
  %1 = sub i64 0, %0
  call void @main.printint(i64 %1)
  ret void
}

define void @main.printany(%"github.com/goplus/llgo/internal/runtime.eface" %0) {
_llgo_0:
  %1 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %2 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 1)
  %3 = icmp eq ptr %1, %2
  %4 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %5 = ptrtoint ptr %4 to i64
  %6 = trunc i64 %5 to i1
  %7 = alloca { i1, i1 }, align 8
  %8 = getelementptr inbounds { i1, i1 }, ptr %7, i32 0, i32 0
  store i1 %6, ptr %8, align 1
  %9 = getelementptr inbounds { i1, i1 }, ptr %7, i32 0, i32 1
  store i1 true, ptr %9, align 1
  %10 = load { i1, i1 }, ptr %7, align 1
  %11 = alloca { i1, i1 }, align 8
  %12 = getelementptr inbounds { i1, i1 }, ptr %11, i32 0, i32 0
  store i1 false, ptr %12, align 1
  %13 = getelementptr inbounds { i1, i1 }, ptr %11, i32 0, i32 1
  store i1 false, ptr %13, align 1
  %14 = load { i1, i1 }, ptr %11, align 1
  %15 = select i1 %3, { i1, i1 } %10, { i1, i1 } %14
  %16 = extractvalue { i1, i1 } %15, 0
  %17 = extractvalue { i1, i1 } %15, 1
  br i1 %17, label %_llgo_2, label %_llgo_3

_llgo_1:                                          ; preds = %_llgo_30, %_llgo_29, %_llgo_28, %_llgo_26, %_llgo_24, %_llgo_22, %_llgo_20, %_llgo_18, %_llgo_16, %_llgo_14, %_llgo_12, %_llgo_10, %_llgo_8, %_llgo_6, %_llgo_4, %_llgo_2
  ret void

_llgo_2:                                          ; preds = %_llgo_0
  call void @main.printbool(i1 %16)
  br label %_llgo_1

_llgo_3:                                          ; preds = %_llgo_0
  %18 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %19 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 2)
  %20 = icmp eq ptr %18, %19
  %21 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %22 = ptrtoint ptr %21 to i64
  %23 = alloca { i64, i1 }, align 8
  %24 = getelementptr inbounds { i64, i1 }, ptr %23, i32 0, i32 0
  store i64 %22, ptr %24, align 4
  %25 = getelementptr inbounds { i64, i1 }, ptr %23, i32 0, i32 1
  store i1 true, ptr %25, align 1
  %26 = load { i64, i1 }, ptr %23, align 4
  %27 = alloca { i64, i1 }, align 8
  %28 = getelementptr inbounds { i64, i1 }, ptr %27, i32 0, i32 0
  store i64 0, ptr %28, align 4
  %29 = getelementptr inbounds { i64, i1 }, ptr %27, i32 0, i32 1
  store i1 false, ptr %29, align 1
  %30 = load { i64, i1 }, ptr %27, align 4
  %31 = select i1 %20, { i64, i1 } %26, { i64, i1 } %30
  %32 = extractvalue { i64, i1 } %31, 0
  %33 = extractvalue { i64, i1 } %31, 1
  br i1 %33, label %_llgo_4, label %_llgo_5

_llgo_4:                                          ; preds = %_llgo_3
  call void @main.printint(i64 %32)
  br label %_llgo_1

_llgo_5:                                          ; preds = %_llgo_3
  %34 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %35 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 3)
  %36 = icmp eq ptr %34, %35
  %37 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %38 = ptrtoint ptr %37 to i64
  %39 = trunc i64 %38 to i8
  %40 = alloca { i8, i1 }, align 8
  %41 = getelementptr inbounds { i8, i1 }, ptr %40, i32 0, i32 0
  store i8 %39, ptr %41, align 1
  %42 = getelementptr inbounds { i8, i1 }, ptr %40, i32 0, i32 1
  store i1 true, ptr %42, align 1
  %43 = load { i8, i1 }, ptr %40, align 1
  %44 = alloca { i8, i1 }, align 8
  %45 = getelementptr inbounds { i8, i1 }, ptr %44, i32 0, i32 0
  store i8 0, ptr %45, align 1
  %46 = getelementptr inbounds { i8, i1 }, ptr %44, i32 0, i32 1
  store i1 false, ptr %46, align 1
  %47 = load { i8, i1 }, ptr %44, align 1
  %48 = select i1 %36, { i8, i1 } %43, { i8, i1 } %47
  %49 = extractvalue { i8, i1 } %48, 0
  %50 = extractvalue { i8, i1 } %48, 1
  br i1 %50, label %_llgo_6, label %_llgo_7

_llgo_6:                                          ; preds = %_llgo_5
  %51 = sext i8 %49 to i64
  call void @main.printint(i64 %51)
  br label %_llgo_1

_llgo_7:                                          ; preds = %_llgo_5
  %52 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %53 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 4)
  %54 = icmp eq ptr %52, %53
  %55 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %56 = ptrtoint ptr %55 to i64
  %57 = trunc i64 %56 to i16
  %58 = alloca { i16, i1 }, align 8
  %59 = getelementptr inbounds { i16, i1 }, ptr %58, i32 0, i32 0
  store i16 %57, ptr %59, align 2
  %60 = getelementptr inbounds { i16, i1 }, ptr %58, i32 0, i32 1
  store i1 true, ptr %60, align 1
  %61 = load { i16, i1 }, ptr %58, align 2
  %62 = alloca { i16, i1 }, align 8
  %63 = getelementptr inbounds { i16, i1 }, ptr %62, i32 0, i32 0
  store i16 0, ptr %63, align 2
  %64 = getelementptr inbounds { i16, i1 }, ptr %62, i32 0, i32 1
  store i1 false, ptr %64, align 1
  %65 = load { i16, i1 }, ptr %62, align 2
  %66 = select i1 %54, { i16, i1 } %61, { i16, i1 } %65
  %67 = extractvalue { i16, i1 } %66, 0
  %68 = extractvalue { i16, i1 } %66, 1
  br i1 %68, label %_llgo_8, label %_llgo_9

_llgo_8:                                          ; preds = %_llgo_7
  %69 = sext i16 %67 to i64
  call void @main.printint(i64 %69)
  br label %_llgo_1

_llgo_9:                                          ; preds = %_llgo_7
  %70 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %71 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 5)
  %72 = icmp eq ptr %70, %71
  %73 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %74 = ptrtoint ptr %73 to i64
  %75 = trunc i64 %74 to i32
  %76 = alloca { i32, i1 }, align 8
  %77 = getelementptr inbounds { i32, i1 }, ptr %76, i32 0, i32 0
  store i32 %75, ptr %77, align 4
  %78 = getelementptr inbounds { i32, i1 }, ptr %76, i32 0, i32 1
  store i1 true, ptr %78, align 1
  %79 = load { i32, i1 }, ptr %76, align 4
  %80 = alloca { i32, i1 }, align 8
  %81 = getelementptr inbounds { i32, i1 }, ptr %80, i32 0, i32 0
  store i32 0, ptr %81, align 4
  %82 = getelementptr inbounds { i32, i1 }, ptr %80, i32 0, i32 1
  store i1 false, ptr %82, align 1
  %83 = load { i32, i1 }, ptr %80, align 4
  %84 = select i1 %72, { i32, i1 } %79, { i32, i1 } %83
  %85 = extractvalue { i32, i1 } %84, 0
  %86 = extractvalue { i32, i1 } %84, 1
  br i1 %86, label %_llgo_10, label %_llgo_11

_llgo_10:                                         ; preds = %_llgo_9
  %87 = sext i32 %85 to i64
  call void @main.printint(i64 %87)
  br label %_llgo_1

_llgo_11:                                         ; preds = %_llgo_9
  %88 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %89 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 6)
  %90 = icmp eq ptr %88, %89
  %91 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %92 = ptrtoint ptr %91 to i64
  %93 = alloca { i64, i1 }, align 8
  %94 = getelementptr inbounds { i64, i1 }, ptr %93, i32 0, i32 0
  store i64 %92, ptr %94, align 4
  %95 = getelementptr inbounds { i64, i1 }, ptr %93, i32 0, i32 1
  store i1 true, ptr %95, align 1
  %96 = load { i64, i1 }, ptr %93, align 4
  %97 = alloca { i64, i1 }, align 8
  %98 = getelementptr inbounds { i64, i1 }, ptr %97, i32 0, i32 0
  store i64 0, ptr %98, align 4
  %99 = getelementptr inbounds { i64, i1 }, ptr %97, i32 0, i32 1
  store i1 false, ptr %99, align 1
  %100 = load { i64, i1 }, ptr %97, align 4
  %101 = select i1 %90, { i64, i1 } %96, { i64, i1 } %100
  %102 = extractvalue { i64, i1 } %101, 0
  %103 = extractvalue { i64, i1 } %101, 1
  br i1 %103, label %_llgo_12, label %_llgo_13

_llgo_12:                                         ; preds = %_llgo_11
  call void @main.printint(i64 %102)
  br label %_llgo_1

_llgo_13:                                         ; preds = %_llgo_11
  %104 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %105 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 7)
  %106 = icmp eq ptr %104, %105
  %107 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %108 = ptrtoint ptr %107 to i64
  %109 = alloca { i64, i1 }, align 8
  %110 = getelementptr inbounds { i64, i1 }, ptr %109, i32 0, i32 0
  store i64 %108, ptr %110, align 4
  %111 = getelementptr inbounds { i64, i1 }, ptr %109, i32 0, i32 1
  store i1 true, ptr %111, align 1
  %112 = load { i64, i1 }, ptr %109, align 4
  %113 = alloca { i64, i1 }, align 8
  %114 = getelementptr inbounds { i64, i1 }, ptr %113, i32 0, i32 0
  store i64 0, ptr %114, align 4
  %115 = getelementptr inbounds { i64, i1 }, ptr %113, i32 0, i32 1
  store i1 false, ptr %115, align 1
  %116 = load { i64, i1 }, ptr %113, align 4
  %117 = select i1 %106, { i64, i1 } %112, { i64, i1 } %116
  %118 = extractvalue { i64, i1 } %117, 0
  %119 = extractvalue { i64, i1 } %117, 1
  br i1 %119, label %_llgo_14, label %_llgo_15

_llgo_14:                                         ; preds = %_llgo_13
  call void @main.printuint(i64 %118)
  br label %_llgo_1

_llgo_15:                                         ; preds = %_llgo_13
  %120 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %121 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 8)
  %122 = icmp eq ptr %120, %121
  %123 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %124 = ptrtoint ptr %123 to i64
  %125 = trunc i64 %124 to i8
  %126 = alloca { i8, i1 }, align 8
  %127 = getelementptr inbounds { i8, i1 }, ptr %126, i32 0, i32 0
  store i8 %125, ptr %127, align 1
  %128 = getelementptr inbounds { i8, i1 }, ptr %126, i32 0, i32 1
  store i1 true, ptr %128, align 1
  %129 = load { i8, i1 }, ptr %126, align 1
  %130 = alloca { i8, i1 }, align 8
  %131 = getelementptr inbounds { i8, i1 }, ptr %130, i32 0, i32 0
  store i8 0, ptr %131, align 1
  %132 = getelementptr inbounds { i8, i1 }, ptr %130, i32 0, i32 1
  store i1 false, ptr %132, align 1
  %133 = load { i8, i1 }, ptr %130, align 1
  %134 = select i1 %122, { i8, i1 } %129, { i8, i1 } %133
  %135 = extractvalue { i8, i1 } %134, 0
  %136 = extractvalue { i8, i1 } %134, 1
  br i1 %136, label %_llgo_16, label %_llgo_17

_llgo_16:                                         ; preds = %_llgo_15
  %137 = zext i8 %135 to i64
  call void @main.printuint(i64 %137)
  br label %_llgo_1

_llgo_17:                                         ; preds = %_llgo_15
  %138 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %139 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 9)
  %140 = icmp eq ptr %138, %139
  %141 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %142 = ptrtoint ptr %141 to i64
  %143 = trunc i64 %142 to i16
  %144 = alloca { i16, i1 }, align 8
  %145 = getelementptr inbounds { i16, i1 }, ptr %144, i32 0, i32 0
  store i16 %143, ptr %145, align 2
  %146 = getelementptr inbounds { i16, i1 }, ptr %144, i32 0, i32 1
  store i1 true, ptr %146, align 1
  %147 = load { i16, i1 }, ptr %144, align 2
  %148 = alloca { i16, i1 }, align 8
  %149 = getelementptr inbounds { i16, i1 }, ptr %148, i32 0, i32 0
  store i16 0, ptr %149, align 2
  %150 = getelementptr inbounds { i16, i1 }, ptr %148, i32 0, i32 1
  store i1 false, ptr %150, align 1
  %151 = load { i16, i1 }, ptr %148, align 2
  %152 = select i1 %140, { i16, i1 } %147, { i16, i1 } %151
  %153 = extractvalue { i16, i1 } %152, 0
  %154 = extractvalue { i16, i1 } %152, 1
  br i1 %154, label %_llgo_18, label %_llgo_19

_llgo_18:                                         ; preds = %_llgo_17
  %155 = zext i16 %153 to i64
  call void @main.printuint(i64 %155)
  br label %_llgo_1

_llgo_19:                                         ; preds = %_llgo_17
  %156 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %157 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 10)
  %158 = icmp eq ptr %156, %157
  %159 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %160 = ptrtoint ptr %159 to i64
  %161 = trunc i64 %160 to i32
  %162 = alloca { i32, i1 }, align 8
  %163 = getelementptr inbounds { i32, i1 }, ptr %162, i32 0, i32 0
  store i32 %161, ptr %163, align 4
  %164 = getelementptr inbounds { i32, i1 }, ptr %162, i32 0, i32 1
  store i1 true, ptr %164, align 1
  %165 = load { i32, i1 }, ptr %162, align 4
  %166 = alloca { i32, i1 }, align 8
  %167 = getelementptr inbounds { i32, i1 }, ptr %166, i32 0, i32 0
  store i32 0, ptr %167, align 4
  %168 = getelementptr inbounds { i32, i1 }, ptr %166, i32 0, i32 1
  store i1 false, ptr %168, align 1
  %169 = load { i32, i1 }, ptr %166, align 4
  %170 = select i1 %158, { i32, i1 } %165, { i32, i1 } %169
  %171 = extractvalue { i32, i1 } %170, 0
  %172 = extractvalue { i32, i1 } %170, 1
  br i1 %172, label %_llgo_20, label %_llgo_21

_llgo_20:                                         ; preds = %_llgo_19
  %173 = zext i32 %171 to i64
  call void @main.printuint(i64 %173)
  br label %_llgo_1

_llgo_21:                                         ; preds = %_llgo_19
  %174 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %175 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 11)
  %176 = icmp eq ptr %174, %175
  %177 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %178 = ptrtoint ptr %177 to i64
  %179 = alloca { i64, i1 }, align 8
  %180 = getelementptr inbounds { i64, i1 }, ptr %179, i32 0, i32 0
  store i64 %178, ptr %180, align 4
  %181 = getelementptr inbounds { i64, i1 }, ptr %179, i32 0, i32 1
  store i1 true, ptr %181, align 1
  %182 = load { i64, i1 }, ptr %179, align 4
  %183 = alloca { i64, i1 }, align 8
  %184 = getelementptr inbounds { i64, i1 }, ptr %183, i32 0, i32 0
  store i64 0, ptr %184, align 4
  %185 = getelementptr inbounds { i64, i1 }, ptr %183, i32 0, i32 1
  store i1 false, ptr %185, align 1
  %186 = load { i64, i1 }, ptr %183, align 4
  %187 = select i1 %176, { i64, i1 } %182, { i64, i1 } %186
  %188 = extractvalue { i64, i1 } %187, 0
  %189 = extractvalue { i64, i1 } %187, 1
  br i1 %189, label %_llgo_22, label %_llgo_23

_llgo_22:                                         ; preds = %_llgo_21
  call void @main.printuint(i64 %188)
  br label %_llgo_1

_llgo_23:                                         ; preds = %_llgo_21
  %190 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %191 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 12)
  %192 = icmp eq ptr %190, %191
  %193 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %194 = ptrtoint ptr %193 to i64
  %195 = alloca { i64, i1 }, align 8
  %196 = getelementptr inbounds { i64, i1 }, ptr %195, i32 0, i32 0
  store i64 %194, ptr %196, align 4
  %197 = getelementptr inbounds { i64, i1 }, ptr %195, i32 0, i32 1
  store i1 true, ptr %197, align 1
  %198 = load { i64, i1 }, ptr %195, align 4
  %199 = alloca { i64, i1 }, align 8
  %200 = getelementptr inbounds { i64, i1 }, ptr %199, i32 0, i32 0
  store i64 0, ptr %200, align 4
  %201 = getelementptr inbounds { i64, i1 }, ptr %199, i32 0, i32 1
  store i1 false, ptr %201, align 1
  %202 = load { i64, i1 }, ptr %199, align 4
  %203 = select i1 %192, { i64, i1 } %198, { i64, i1 } %202
  %204 = extractvalue { i64, i1 } %203, 0
  %205 = extractvalue { i64, i1 } %203, 1
  br i1 %205, label %_llgo_24, label %_llgo_25

_llgo_24:                                         ; preds = %_llgo_23
  call void @main.printuint(i64 %204)
  br label %_llgo_1

_llgo_25:                                         ; preds = %_llgo_23
  %206 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %207 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 13)
  %208 = icmp eq ptr %206, %207
  %209 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %210 = ptrtoint ptr %209 to i64
  %211 = bitcast i64 %210 to float
  %212 = alloca { float, i1 }, align 8
  %213 = getelementptr inbounds { float, i1 }, ptr %212, i32 0, i32 0
  store float %211, ptr %213, align 4
  %214 = getelementptr inbounds { float, i1 }, ptr %212, i32 0, i32 1
  store i1 true, ptr %214, align 1
  %215 = load { float, i1 }, ptr %212, align 4
  %216 = alloca { float, i1 }, align 8
  %217 = getelementptr inbounds { float, i1 }, ptr %216, i32 0, i32 0
  store double 0.000000e+00, ptr %217, align 8
  %218 = getelementptr inbounds { float, i1 }, ptr %216, i32 0, i32 1
  store i1 false, ptr %218, align 1
  %219 = load { float, i1 }, ptr %216, align 4
  %220 = select i1 %208, { float, i1 } %215, { float, i1 } %219
  %221 = extractvalue { float, i1 } %220, 0
  %222 = extractvalue { float, i1 } %220, 1
  br i1 %222, label %_llgo_26, label %_llgo_27

_llgo_26:                                         ; preds = %_llgo_25
  %223 = fpext float %221 to double
  call void @main.printfloat(double %223)
  br label %_llgo_1

_llgo_27:                                         ; preds = %_llgo_25
  %224 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %225 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 14)
  %226 = icmp eq ptr %224, %225
  %227 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %228 = ptrtoint ptr %227 to i64
  %229 = bitcast i64 %228 to double
  %230 = alloca { double, i1 }, align 8
  %231 = getelementptr inbounds { double, i1 }, ptr %230, i32 0, i32 0
  store double %229, ptr %231, align 8
  %232 = getelementptr inbounds { double, i1 }, ptr %230, i32 0, i32 1
  store i1 true, ptr %232, align 1
  %233 = load { double, i1 }, ptr %230, align 8
  %234 = alloca { double, i1 }, align 8
  %235 = getelementptr inbounds { double, i1 }, ptr %234, i32 0, i32 0
  store double 0.000000e+00, ptr %235, align 8
  %236 = getelementptr inbounds { double, i1 }, ptr %234, i32 0, i32 1
  store i1 false, ptr %236, align 1
  %237 = load { double, i1 }, ptr %234, align 8
  %238 = select i1 %226, { double, i1 } %233, { double, i1 } %237
  %239 = extractvalue { double, i1 } %238, 0
  %240 = extractvalue { double, i1 } %238, 1
  br i1 %240, label %_llgo_28, label %_llgo_29

_llgo_28:                                         ; preds = %_llgo_27
  call void @main.printfloat(double %239)
  br label %_llgo_1

_llgo_29:                                         ; preds = %_llgo_27
  %241 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 0
  %242 = call ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64 24)
  %243 = icmp eq ptr %241, %242
  %244 = extractvalue %"github.com/goplus/llgo/internal/runtime.eface" %0, 1
  %245 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %244, align 8
  %246 = alloca { %"github.com/goplus/llgo/internal/runtime.String", i1 }, align 8
  %247 = getelementptr inbounds { %"github.com/goplus/llgo/internal/runtime.String", i1 }, ptr %246, i32 0, i32 0
  store %"github.com/goplus/llgo/internal/runtime.String" %245, ptr %247, align 8
  %248 = getelementptr inbounds { %"github.com/goplus/llgo/internal/runtime.String", i1 }, ptr %246, i32 0, i32 1
  store i1 true, ptr %248, align 1
  %249 = load { %"github.com/goplus/llgo/internal/runtime.String", i1 }, ptr %246, align 8
  %250 = alloca { %"github.com/goplus/llgo/internal/runtime.String", i1 }, align 8
  %251 = getelementptr inbounds { %"github.com/goplus/llgo/internal/runtime.String", i1 }, ptr %250, i32 0, i32 0
  store { ptr, i64 } zeroinitializer, ptr %251, align 8
  %252 = getelementptr inbounds { %"github.com/goplus/llgo/internal/runtime.String", i1 }, ptr %250, i32 0, i32 1
  store i1 false, ptr %252, align 1
  %253 = load { %"github.com/goplus/llgo/internal/runtime.String", i1 }, ptr %250, align 8
  %254 = select i1 %243, { %"github.com/goplus/llgo/internal/runtime.String", i1 } %249, { %"github.com/goplus/llgo/internal/runtime.String", i1 } %253
  %255 = extractvalue { %"github.com/goplus/llgo/internal/runtime.String", i1 } %254, 0
  %256 = extractvalue { %"github.com/goplus/llgo/internal/runtime.String", i1 } %254, 1
  br i1 %256, label %_llgo_30, label %_llgo_1

_llgo_30:                                         ; preds = %_llgo_29
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %255)
  br label %_llgo_1
}

define void @main.printbool(i1 %0) {
_llgo_0:
  br i1 %0, label %_llgo_1, label %_llgo_3

_llgo_1:                                          ; preds = %_llgo_0
  %1 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %2 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %1, i32 0, i32 0
  store ptr @5, ptr %2, align 8
  %3 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %1, i32 0, i32 1
  store i64 4, ptr %3, align 4
  %4 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %1, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %4)
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_3, %_llgo_1
  ret void

_llgo_3:                                          ; preds = %_llgo_0
  %5 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %6 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %5, i32 0, i32 0
  store ptr @6, ptr %6, align 8
  %7 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %5, i32 0, i32 1
  store i64 5, ptr %7, align 4
  %8 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %5, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %8)
  br label %_llgo_2
}

define void @main.printfloat(double %0) {
_llgo_0:
  %1 = fcmp one double %0, %0
  br i1 %1, label %_llgo_1, label %_llgo_3

_llgo_1:                                          ; preds = %_llgo_0
  %2 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %3 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %2, i32 0, i32 0
  store ptr @7, ptr %3, align 8
  %4 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %2, i32 0, i32 1
  store i64 3, ptr %4, align 4
  %5 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %2, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %5)
  ret void

_llgo_2:                                          ; preds = %_llgo_7
  %6 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %7 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %6, i32 0, i32 0
  store ptr @8, ptr %7, align 8
  %8 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %6, i32 0, i32 1
  store i64 4, ptr %8, align 4
  %9 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %6, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %9)
  ret void

_llgo_3:                                          ; preds = %_llgo_0
  %10 = fadd double %0, %0
  %11 = fcmp oeq double %10, %0
  br i1 %11, label %_llgo_6, label %_llgo_7

_llgo_4:                                          ; preds = %_llgo_10
  %12 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %13 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %12, i32 0, i32 0
  store ptr @9, ptr %13, align 8
  %14 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %12, i32 0, i32 1
  store i64 4, ptr %14, align 4
  %15 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %12, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %15)
  ret void

_llgo_5:                                          ; preds = %_llgo_7
  %16 = fadd double %0, %0
  %17 = fcmp oeq double %16, %0
  br i1 %17, label %_llgo_9, label %_llgo_10

_llgo_6:                                          ; preds = %_llgo_3
  %18 = fcmp ogt double %0, 0.000000e+00
  br label %_llgo_7

_llgo_7:                                          ; preds = %_llgo_6, %_llgo_3
  %19 = phi i1 [ false, %_llgo_3 ], [ %18, %_llgo_6 ]
  br i1 %19, label %_llgo_2, label %_llgo_5

_llgo_8:                                          ; preds = %_llgo_10
  %20 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocZ"(i64 14)
  %21 = getelementptr inbounds i8, ptr %20, i64 0
  store i8 43, ptr %21, align 1
  %22 = fcmp oeq double %0, 0.000000e+00
  br i1 %22, label %_llgo_11, label %_llgo_13

_llgo_9:                                          ; preds = %_llgo_5
  %23 = fcmp olt double %0, 0.000000e+00
  br label %_llgo_10

_llgo_10:                                         ; preds = %_llgo_9, %_llgo_5
  %24 = phi i1 [ false, %_llgo_5 ], [ %23, %_llgo_9 ]
  br i1 %24, label %_llgo_4, label %_llgo_8

_llgo_11:                                         ; preds = %_llgo_8
  %25 = fdiv double 1.000000e+00, %0
  %26 = fcmp olt double %25, 0.000000e+00
  br i1 %26, label %_llgo_14, label %_llgo_12

_llgo_12:                                         ; preds = %_llgo_24, %_llgo_22, %_llgo_14, %_llgo_11
  %27 = phi double [ %0, %_llgo_11 ], [ %45, %_llgo_22 ], [ %0, %_llgo_14 ], [ %51, %_llgo_24 ]
  %28 = phi i64 [ 0, %_llgo_11 ], [ %41, %_llgo_22 ], [ 0, %_llgo_14 ], [ %50, %_llgo_24 ]
  br label %_llgo_27

_llgo_13:                                         ; preds = %_llgo_8
  %29 = fcmp olt double %0, 0.000000e+00
  br i1 %29, label %_llgo_15, label %_llgo_17

_llgo_14:                                         ; preds = %_llgo_11
  %30 = getelementptr inbounds i8, ptr %20, i64 0
  store i8 45, ptr %30, align 1
  br label %_llgo_12

_llgo_15:                                         ; preds = %_llgo_13
  %31 = fneg double %0
  %32 = getelementptr inbounds i8, ptr %20, i64 0
  store i8 45, ptr %32, align 1
  br label %_llgo_17

_llgo_16:                                         ; preds = %_llgo_17
  %33 = add i64 %36, 1
  %34 = fdiv double %35, 1.000000e+01
  br label %_llgo_17

_llgo_17:                                         ; preds = %_llgo_16, %_llgo_15, %_llgo_13
  %35 = phi double [ %0, %_llgo_13 ], [ %34, %_llgo_16 ], [ %31, %_llgo_15 ]
  %36 = phi i64 [ 0, %_llgo_13 ], [ %33, %_llgo_16 ], [ 0, %_llgo_15 ]
  %37 = fcmp oge double %35, 1.000000e+01
  br i1 %37, label %_llgo_16, label %_llgo_20

_llgo_18:                                         ; preds = %_llgo_20
  %38 = sub i64 %41, 1
  %39 = fmul double %40, 1.000000e+01
  br label %_llgo_20

_llgo_19:                                         ; preds = %_llgo_20
  br label %_llgo_23

_llgo_20:                                         ; preds = %_llgo_18, %_llgo_17
  %40 = phi double [ %35, %_llgo_17 ], [ %39, %_llgo_18 ]
  %41 = phi i64 [ %36, %_llgo_17 ], [ %38, %_llgo_18 ]
  %42 = fcmp olt double %40, 1.000000e+00
  br i1 %42, label %_llgo_18, label %_llgo_19

_llgo_21:                                         ; preds = %_llgo_23
  %43 = fdiv double %47, 1.000000e+01
  %44 = add i64 %48, 1
  br label %_llgo_23

_llgo_22:                                         ; preds = %_llgo_23
  %45 = fadd double %40, %47
  %46 = fcmp oge double %45, 1.000000e+01
  br i1 %46, label %_llgo_24, label %_llgo_12

_llgo_23:                                         ; preds = %_llgo_21, %_llgo_19
  %47 = phi double [ 5.000000e+00, %_llgo_19 ], [ %43, %_llgo_21 ]
  %48 = phi i64 [ 0, %_llgo_19 ], [ %44, %_llgo_21 ]
  %49 = icmp slt i64 %48, 7
  br i1 %49, label %_llgo_21, label %_llgo_22

_llgo_24:                                         ; preds = %_llgo_22
  %50 = add i64 %41, 1
  %51 = fdiv double %45, 1.000000e+01
  br label %_llgo_12

_llgo_25:                                         ; preds = %_llgo_27
  %52 = fptosi double %69 to i64
  %53 = add i64 %70, 2
  %54 = add i64 %52, 48
  %55 = trunc i64 %54 to i8
  %56 = icmp slt i64 %53, 0
  call void @"github.com/goplus/llgo/internal/runtime.AssertIndexRange"(i1 %56)
  %57 = getelementptr inbounds i8, ptr %20, i64 %53
  store i8 %55, ptr %57, align 1
  %58 = sitofp i64 %52 to double
  %59 = fsub double %69, %58
  %60 = fmul double %59, 1.000000e+01
  %61 = add i64 %70, 1
  br label %_llgo_27

_llgo_26:                                         ; preds = %_llgo_27
  %62 = getelementptr inbounds i8, ptr %20, i64 2
  %63 = load i8, ptr %62, align 1
  %64 = getelementptr inbounds i8, ptr %20, i64 1
  store i8 %63, ptr %64, align 1
  %65 = getelementptr inbounds i8, ptr %20, i64 2
  store i8 46, ptr %65, align 1
  %66 = getelementptr inbounds i8, ptr %20, i64 9
  store i8 101, ptr %66, align 1
  %67 = getelementptr inbounds i8, ptr %20, i64 10
  store i8 43, ptr %67, align 1
  %68 = icmp slt i64 %28, 0
  br i1 %68, label %_llgo_28, label %_llgo_29

_llgo_27:                                         ; preds = %_llgo_25, %_llgo_12
  %69 = phi double [ %27, %_llgo_12 ], [ %60, %_llgo_25 ]
  %70 = phi i64 [ 0, %_llgo_12 ], [ %61, %_llgo_25 ]
  %71 = icmp slt i64 %70, 7
  br i1 %71, label %_llgo_25, label %_llgo_26

_llgo_28:                                         ; preds = %_llgo_26
  %72 = sub i64 0, %28
  %73 = getelementptr inbounds i8, ptr %20, i64 10
  store i8 45, ptr %73, align 1
  br label %_llgo_29

_llgo_29:                                         ; preds = %_llgo_28, %_llgo_26
  %74 = phi i64 [ %28, %_llgo_26 ], [ %72, %_llgo_28 ]
  %75 = sdiv i64 %74, 100
  %76 = trunc i64 %75 to i8
  %77 = add i8 %76, 48
  %78 = getelementptr inbounds i8, ptr %20, i64 11
  store i8 %77, ptr %78, align 1
  %79 = sdiv i64 %74, 10
  %80 = trunc i64 %79 to i8
  %81 = urem i8 %80, 10
  %82 = add i8 %81, 48
  %83 = getelementptr inbounds i8, ptr %20, i64 12
  store i8 %82, ptr %83, align 1
  %84 = srem i64 %74, 10
  %85 = trunc i64 %84 to i8
  %86 = add i8 %85, 48
  %87 = getelementptr inbounds i8, ptr %20, i64 13
  store i8 %86, ptr %87, align 1
  %88 = call %"github.com/goplus/llgo/internal/runtime.Slice" @"github.com/goplus/llgo/internal/runtime.NewSlice3"(ptr %20, i64 1, i64 14, i64 0, i64 14, i64 14)
  call void @main.gwrite(%"github.com/goplus/llgo/internal/runtime.Slice" %88)
  ret void
}

define void @main.printhex(i64 %0) {
_llgo_0:
  %1 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocZ"(i64 100)
  br label %_llgo_3

_llgo_1:                                          ; preds = %_llgo_3
  %2 = urem i64 %20, 16
  %3 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %4 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %3, i32 0, i32 0
  store ptr @10, ptr %4, align 8
  %5 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %3, i32 0, i32 1
  store i64 16, ptr %5, align 4
  %6 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %3, align 8
  %7 = extractvalue %"github.com/goplus/llgo/internal/runtime.String" %6, 0
  %8 = getelementptr inbounds i8, ptr %7, i64 %2
  %9 = load i8, ptr %8, align 1
  %10 = icmp slt i64 %21, 0
  call void @"github.com/goplus/llgo/internal/runtime.AssertIndexRange"(i1 %10)
  %11 = getelementptr inbounds i8, ptr %1, i64 %21
  store i8 %9, ptr %11, align 1
  %12 = icmp ult i64 %20, 16
  br i1 %12, label %_llgo_5, label %_llgo_4

_llgo_2:                                          ; preds = %_llgo_5, %_llgo_3
  %13 = sub i64 %21, 1
  %14 = icmp slt i64 %13, 0
  call void @"github.com/goplus/llgo/internal/runtime.AssertIndexRange"(i1 %14)
  %15 = getelementptr inbounds i8, ptr %1, i64 %13
  store i8 120, ptr %15, align 1
  %16 = sub i64 %13, 1
  %17 = icmp slt i64 %16, 0
  call void @"github.com/goplus/llgo/internal/runtime.AssertIndexRange"(i1 %17)
  %18 = getelementptr inbounds i8, ptr %1, i64 %16
  store i8 48, ptr %18, align 1
  %19 = call %"github.com/goplus/llgo/internal/runtime.Slice" @"github.com/goplus/llgo/internal/runtime.NewSlice3"(ptr %1, i64 1, i64 100, i64 %16, i64 100, i64 100)
  call void @main.gwrite(%"github.com/goplus/llgo/internal/runtime.Slice" %19)
  ret void

_llgo_3:                                          ; preds = %_llgo_4, %_llgo_0
  %20 = phi i64 [ %0, %_llgo_0 ], [ %23, %_llgo_4 ]
  %21 = phi i64 [ 99, %_llgo_0 ], [ %24, %_llgo_4 ]
  %22 = icmp sgt i64 %21, 0
  br i1 %22, label %_llgo_1, label %_llgo_2

_llgo_4:                                          ; preds = %_llgo_5, %_llgo_1
  %23 = udiv i64 %20, 16
  %24 = sub i64 %21, 1
  br label %_llgo_3

_llgo_5:                                          ; preds = %_llgo_1
  %25 = sub i64 100, %21
  %26 = load i64, ptr @main.minhexdigits, align 4
  %27 = icmp sge i64 %25, %26
  br i1 %27, label %_llgo_2, label %_llgo_4
}

define void @main.printint(i64 %0) {
_llgo_0:
  %1 = icmp slt i64 %0, 0
  br i1 %1, label %_llgo_1, label %_llgo_2

_llgo_1:                                          ; preds = %_llgo_0
  %2 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %3 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %2, i32 0, i32 0
  store ptr @11, ptr %3, align 8
  %4 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %2, i32 0, i32 1
  store i64 1, ptr %4, align 4
  %5 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %2, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %5)
  %6 = sub i64 0, %0
  br label %_llgo_2

_llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
  %7 = phi i64 [ %0, %_llgo_0 ], [ %6, %_llgo_1 ]
  call void @main.printuint(i64 %7)
  ret void
}

define void @main.println(%"github.com/goplus/llgo/internal/runtime.Slice" %0) {
_llgo_0:
  %1 = extractvalue %"github.com/goplus/llgo/internal/runtime.Slice" %0, 1
  br label %_llgo_1

_llgo_1:                                          ; preds = %_llgo_5, %_llgo_0
  %2 = phi i64 [ -1, %_llgo_0 ], [ %3, %_llgo_5 ]
  %3 = add i64 %2, 1
  %4 = icmp slt i64 %3, %1
  br i1 %4, label %_llgo_2, label %_llgo_3

_llgo_2:                                          ; preds = %_llgo_1
  %5 = icmp slt i64 %3, 0
  call void @"github.com/goplus/llgo/internal/runtime.AssertIndexRange"(i1 %5)
  %6 = extractvalue %"github.com/goplus/llgo/internal/runtime.Slice" %0, 0
  %7 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.eface", ptr %6, i64 %3
  %8 = load %"github.com/goplus/llgo/internal/runtime.eface", ptr %7, align 8
  %9 = icmp ne i64 %3, 0
  br i1 %9, label %_llgo_4, label %_llgo_5

_llgo_3:                                          ; preds = %_llgo_1
  call void @main.printnl()
  ret void

_llgo_4:                                          ; preds = %_llgo_2
  %10 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %11 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %10, i32 0, i32 0
  store ptr @12, ptr %11, align 8
  %12 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %10, i32 0, i32 1
  store i64 1, ptr %12, align 4
  %13 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %10, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %13)
  br label %_llgo_5

_llgo_5:                                          ; preds = %_llgo_4, %_llgo_2
  call void @main.printany(%"github.com/goplus/llgo/internal/runtime.eface" %8)
  br label %_llgo_1
}

define void @main.printnl() {
_llgo_0:
  %0 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %1 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %0, i32 0, i32 0
  store ptr @13, ptr %1, align 8
  %2 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %0, i32 0, i32 1
  store i64 1, ptr %2, align 4
  %3 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %0, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %3)
  ret void
}

define void @main.printsp() {
_llgo_0:
  %0 = alloca %"github.com/goplus/llgo/internal/runtime.String", align 8
  %1 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %0, i32 0, i32 0
  store ptr @14, ptr %1, align 8
  %2 = getelementptr inbounds %"github.com/goplus/llgo/internal/runtime.String", ptr %0, i32 0, i32 1
  store i64 1, ptr %2, align 4
  %3 = load %"github.com/goplus/llgo/internal/runtime.String", ptr %0, align 8
  call void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %3)
  ret void
}

define void @main.printstring(%"github.com/goplus/llgo/internal/runtime.String" %0) {
_llgo_0:
  %1 = call %"github.com/goplus/llgo/internal/runtime.Slice" @main.bytes(%"github.com/goplus/llgo/internal/runtime.String" %0)
  call void @main.gwrite(%"github.com/goplus/llgo/internal/runtime.Slice" %1)
  ret void
}

define void @main.printuint(i64 %0) {
_llgo_0:
  %1 = call ptr @"github.com/goplus/llgo/internal/runtime.AllocZ"(i64 100)
  br label %_llgo_3

_llgo_1:                                          ; preds = %_llgo_3
  %2 = urem i64 %9, 10
  %3 = add i64 %2, 48
  %4 = trunc i64 %3 to i8
  %5 = icmp slt i64 %10, 0
  call void @"github.com/goplus/llgo/internal/runtime.AssertIndexRange"(i1 %5)
  %6 = getelementptr inbounds i8, ptr %1, i64 %10
  store i8 %4, ptr %6, align 1
  %7 = icmp ult i64 %9, 10
  br i1 %7, label %_llgo_2, label %_llgo_4

_llgo_2:                                          ; preds = %_llgo_3, %_llgo_1
  %8 = call %"github.com/goplus/llgo/internal/runtime.Slice" @"github.com/goplus/llgo/internal/runtime.NewSlice3"(ptr %1, i64 1, i64 100, i64 %10, i64 100, i64 100)
  call void @main.gwrite(%"github.com/goplus/llgo/internal/runtime.Slice" %8)
  ret void

_llgo_3:                                          ; preds = %_llgo_4, %_llgo_0
  %9 = phi i64 [ %0, %_llgo_0 ], [ %12, %_llgo_4 ]
  %10 = phi i64 [ 99, %_llgo_0 ], [ %13, %_llgo_4 ]
  %11 = icmp sgt i64 %10, 0
  br i1 %11, label %_llgo_1, label %_llgo_2

_llgo_4:                                          ; preds = %_llgo_1
  %12 = udiv i64 %9, 10
  %13 = sub i64 %10, 1
  br label %_llgo_3
}

define void @main.prinusub(i64 %0) {
_llgo_0:
  %1 = sub i64 0, %0
  call void @main.printuint(i64 %1)
  ret void
}

define void @main.prinxor(i64 %0) {
_llgo_0:
  %1 = xor i64 %0, -1
  call void @main.printint(i64 %1)
  ret void
}

define ptr @main.stringStructOf(ptr %0) {
_llgo_0:
  ret ptr %0
}

declare ptr @"github.com/goplus/llgo/internal/runtime.AllocZ"(i64)

declare i32 @printf(ptr, ...)

declare void @"github.com/goplus/llgo/internal/runtime.init"()

declare ptr @"github.com/goplus/llgo/internal/runtime.Basic"(i64)

declare ptr @"github.com/goplus/llgo/internal/runtime.AllocU"(i64)

declare %"github.com/goplus/llgo/internal/runtime.Slice" @"github.com/goplus/llgo/internal/runtime.NewSlice3"(ptr, i64, i64, i64, i64, i64)

declare void @"github.com/goplus/llgo/internal/runtime.AssertIndexRange"(i1)
