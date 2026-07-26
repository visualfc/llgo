// LITTEST
package main

import (
	"unsafe"

	"github.com/goplus/llgo/runtime/abi"
)

// CHECK: {{^}}@121 = private unnamed_addr constant [13 x i8] c"error field 0", align 1{{$}}
// CHECK: {{^}}@122 = private unnamed_addr constant [18 x i8] c"error field 0 elem", align 1{{$}}
// CHECK: {{^}}@123 = private unnamed_addr constant [13 x i8] c"error field 1", align 1{{$}}
// CHECK: {{^}}@124 = private unnamed_addr constant [18 x i8] c"error field 1 elem", align 1{{$}}
// CHECK: {{^}}@125 = private unnamed_addr constant [13 x i8] c"error field 2", align 1{{$}}
// CHECK: {{^}}@126 = private unnamed_addr constant [13 x i8] c"error field 3", align 1{{$}}

// CHECK-LABEL: define void @"{{.*}}/cl/_testrt/abinamed.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testrt/abinamed.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testrt/abinamed.init$guard", align 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/abi.init"()
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

type T struct {
	p *T
	t *abi.Type
	n uintptr
	a []T
}

type eface struct {
	typ  *abi.Type
	data unsafe.Pointer
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testrt/abinamed.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 48)
// CHECK-NEXT:   store %"{{.*}}/cl/_testrt/abinamed.T" zeroinitializer, ptr %0, align 8
// CHECK-NEXT:   %1 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_{{.*}}/cl/_testrt/abinamed.T", ptr undef }, ptr %0, 1
// CHECK-NEXT:   %2 = call ptr @"{{.*}}/cl/_testrt/abinamed.toEface"(%"{{.*}}/runtime/internal/runtime.eface" %1)
// CHECK-NEXT:   %3 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 72)
// CHECK-NEXT:   store %"{{.*}}/runtime/abi.Type" zeroinitializer, ptr %3, align 8
// CHECK-NEXT:   %4 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_{{.*}}/runtime/abi.Type", ptr undef }, ptr %3, 1
// CHECK-NEXT:   %5 = call ptr @"{{.*}}/cl/_testrt/abinamed.toEface"(%"{{.*}}/runtime/internal/runtime.eface" %4)
// CHECK-NEXT:   %6 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %7 = load ptr, ptr %6, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintPointer"(ptr %7)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   %8 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %9 = load ptr, ptr %8, align 8
// CHECK-NEXT:   %10 = getelementptr inbounds %"{{.*}}/runtime/abi.Type", ptr %9, i32 0, i32 10
// CHECK-NEXT:   %11 = load ptr, ptr %10, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintPointer"(ptr %11)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   %12 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %5, i32 0, i32 0
// CHECK-NEXT:   %13 = load ptr, ptr %12, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintPointer"(ptr %13)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   %14 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %5, i32 0, i32 0
// CHECK-NEXT:   %15 = load ptr, ptr %14, align 8
// CHECK-NEXT:   %16 = getelementptr inbounds %"{{.*}}/runtime/abi.Type", ptr %15, i32 0, i32 10
// CHECK-NEXT:   %17 = load ptr, ptr %16, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintPointer"(ptr %17)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   %18 = alloca %"{{.*}}/runtime/abi.StructField", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %18, i8 0, i64 56, i1 false)
// CHECK-NEXT:   %19 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %20 = load ptr, ptr %19, align 8
// CHECK-NEXT:   %21 = call ptr @"{{.*}}/runtime/abi.(*Type).StructType"(ptr %20)
// CHECK-NEXT:   %22 = getelementptr inbounds %"{{.*}}/runtime/abi.StructType", ptr %21, i32 0, i32 2
// CHECK-NEXT:   %23 = load %"{{.*}}/runtime/internal/runtime.Slice", ptr %22, align 8
// CHECK-NEXT:   %24 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %23, 0
// CHECK-NEXT:   %25 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %23, 1
// CHECK-NEXT:   %26 = icmp uge i64 0, %25
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.CheckIndexRange"(i1 %26, i64 0, i1 true, i64 %25)
// CHECK-NEXT:   %27 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %24, i64 0
// CHECK-NEXT:   %28 = load %"{{.*}}/runtime/abi.StructField", ptr %27, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/abi.StructField" %28, ptr %18, align 8
// CHECK-NEXT:   %29 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %18, i32 0, i32 1
// CHECK-NEXT:   %30 = load ptr, ptr %29, align 8
// CHECK-NEXT:   %31 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %32 = load ptr, ptr %31, align 8
// CHECK-NEXT:   %33 = getelementptr inbounds %"{{.*}}/runtime/abi.Type", ptr %32, i32 0, i32 10
// CHECK-NEXT:   %34 = load ptr, ptr %33, align 8
// CHECK-NEXT:   %35 = icmp ne ptr %30, %34
// CHECK-NEXT:   br i1 %35, label %_llgo_1, label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %36 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @121, i64 13 }, ptr %36, align 8
// CHECK-NEXT:   %37 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %36, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %37)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %38 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %18, i32 0, i32 1
// CHECK-NEXT:   %39 = load ptr, ptr %38, align 8
// CHECK-NEXT:   %40 = call ptr @"{{.*}}/runtime/abi.(*Type).Elem"(ptr %39)
// CHECK-NEXT:   %41 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %42 = load ptr, ptr %41, align 8
// CHECK-NEXT:   %43 = icmp ne ptr %40, %42
// CHECK-NEXT:   br i1 %43, label %_llgo_3, label %_llgo_4
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_2
// CHECK-NEXT:   %44 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @122, i64 18 }, ptr %44, align 8
// CHECK-NEXT:   %45 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %44, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %45)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_4:                                          ; preds = %_llgo_2
// CHECK-NEXT:   %46 = alloca %"{{.*}}/runtime/abi.StructField", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %46, i8 0, i64 56, i1 false)
// CHECK-NEXT:   %47 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %48 = load ptr, ptr %47, align 8
// CHECK-NEXT:   %49 = call ptr @"{{.*}}/runtime/abi.(*Type).StructType"(ptr %48)
// CHECK-NEXT:   %50 = getelementptr inbounds %"{{.*}}/runtime/abi.StructType", ptr %49, i32 0, i32 2
// CHECK-NEXT:   %51 = load %"{{.*}}/runtime/internal/runtime.Slice", ptr %50, align 8
// CHECK-NEXT:   %52 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %51, 0
// CHECK-NEXT:   %53 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %51, 1
// CHECK-NEXT:   %54 = icmp uge i64 1, %53
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.CheckIndexRange"(i1 %54, i64 1, i1 true, i64 %53)
// CHECK-NEXT:   %55 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %52, i64 1
// CHECK-NEXT:   %56 = load %"{{.*}}/runtime/abi.StructField", ptr %55, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/abi.StructField" %56, ptr %46, align 8
// CHECK-NEXT:   %57 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %46, i32 0, i32 1
// CHECK-NEXT:   %58 = load ptr, ptr %57, align 8
// CHECK-NEXT:   %59 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %5, i32 0, i32 0
// CHECK-NEXT:   %60 = load ptr, ptr %59, align 8
// CHECK-NEXT:   %61 = getelementptr inbounds %"{{.*}}/runtime/abi.Type", ptr %60, i32 0, i32 10
// CHECK-NEXT:   %62 = load ptr, ptr %61, align 8
// CHECK-NEXT:   %63 = icmp ne ptr %58, %62
// CHECK-NEXT:   br i1 %63, label %_llgo_5, label %_llgo_6
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_5:                                          ; preds = %_llgo_4
// CHECK-NEXT:   %64 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @123, i64 13 }, ptr %64, align 8
// CHECK-NEXT:   %65 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %64, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %65)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_6:                                          ; preds = %_llgo_4
// CHECK-NEXT:   %66 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %46, i32 0, i32 1
// CHECK-NEXT:   %67 = load ptr, ptr %66, align 8
// CHECK-NEXT:   %68 = call ptr @"{{.*}}/runtime/abi.(*Type).Elem"(ptr %67)
// CHECK-NEXT:   %69 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %5, i32 0, i32 0
// CHECK-NEXT:   %70 = load ptr, ptr %69, align 8
// CHECK-NEXT:   %71 = icmp ne ptr %68, %70
// CHECK-NEXT:   br i1 %71, label %_llgo_7, label %_llgo_8
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_7:                                          ; preds = %_llgo_6
// CHECK-NEXT:   %72 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @124, i64 18 }, ptr %72, align 8
// CHECK-NEXT:   %73 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %72, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %73)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_8:                                          ; preds = %_llgo_6
// CHECK-NEXT:   %74 = alloca %"{{.*}}/runtime/abi.StructField", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %74, i8 0, i64 56, i1 false)
// CHECK-NEXT:   %75 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %76 = load ptr, ptr %75, align 8
// CHECK-NEXT:   %77 = call ptr @"{{.*}}/runtime/abi.(*Type).StructType"(ptr %76)
// CHECK-NEXT:   %78 = getelementptr inbounds %"{{.*}}/runtime/abi.StructType", ptr %77, i32 0, i32 2
// CHECK-NEXT:   %79 = load %"{{.*}}/runtime/internal/runtime.Slice", ptr %78, align 8
// CHECK-NEXT:   %80 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %79, 0
// CHECK-NEXT:   %81 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %79, 1
// CHECK-NEXT:   %82 = icmp uge i64 2, %81
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.CheckIndexRange"(i1 %82, i64 2, i1 true, i64 %81)
// CHECK-NEXT:   %83 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %80, i64 2
// CHECK-NEXT:   %84 = load %"{{.*}}/runtime/abi.StructField", ptr %83, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/abi.StructField" %84, ptr %74, align 8
// CHECK-NEXT:   %85 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %74, i32 0, i32 1
// CHECK-NEXT:   %86 = load ptr, ptr %85, align 8
// CHECK-NEXT:   %87 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %5, i32 0, i32 0
// CHECK-NEXT:   %88 = load ptr, ptr %87, align 8
// CHECK-NEXT:   %89 = call ptr @"{{.*}}/runtime/abi.(*Type).StructType"(ptr %88)
// CHECK-NEXT:   %90 = getelementptr inbounds %"{{.*}}/runtime/abi.StructType", ptr %89, i32 0, i32 2
// CHECK-NEXT:   %91 = load %"{{.*}}/runtime/internal/runtime.Slice", ptr %90, align 8
// CHECK-NEXT:   %92 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %91, 0
// CHECK-NEXT:   %93 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %91, 1
// CHECK-NEXT:   %94 = icmp uge i64 0, %93
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.CheckIndexRange"(i1 %94, i64 0, i1 true, i64 %93)
// CHECK-NEXT:   %95 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %92, i64 0
// CHECK-NEXT:   %96 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %95, i32 0, i32 1
// CHECK-NEXT:   %97 = load ptr, ptr %96, align 8
// CHECK-NEXT:   %98 = icmp ne ptr %86, %97
// CHECK-NEXT:   br i1 %98, label %_llgo_9, label %_llgo_10
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_9:                                          ; preds = %_llgo_8
// CHECK-NEXT:   %99 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @125, i64 13 }, ptr %99, align 8
// CHECK-NEXT:   %100 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %99, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %100)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_10:                                         ; preds = %_llgo_8
// CHECK-NEXT:   %101 = alloca %"{{.*}}/runtime/abi.StructField", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %101, i8 0, i64 56, i1 false)
// CHECK-NEXT:   %102 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %103 = load ptr, ptr %102, align 8
// CHECK-NEXT:   %104 = call ptr @"{{.*}}/runtime/abi.(*Type).StructType"(ptr %103)
// CHECK-NEXT:   %105 = getelementptr inbounds %"{{.*}}/runtime/abi.StructType", ptr %104, i32 0, i32 2
// CHECK-NEXT:   %106 = load %"{{.*}}/runtime/internal/runtime.Slice", ptr %105, align 8
// CHECK-NEXT:   %107 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %106, 0
// CHECK-NEXT:   %108 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %106, 1
// CHECK-NEXT:   %109 = icmp uge i64 3, %108
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.CheckIndexRange"(i1 %109, i64 3, i1 true, i64 %108)
// CHECK-NEXT:   %110 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %107, i64 3
// CHECK-NEXT:   %111 = load %"{{.*}}/runtime/abi.StructField", ptr %110, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/abi.StructField" %111, ptr %101, align 8
// CHECK-NEXT:   %112 = getelementptr inbounds %"{{.*}}/runtime/abi.StructField", ptr %101, i32 0, i32 1
// CHECK-NEXT:   %113 = load ptr, ptr %112, align 8
// CHECK-NEXT:   %114 = call ptr @"{{.*}}/runtime/abi.(*Type).Elem"(ptr %113)
// CHECK-NEXT:   %115 = getelementptr inbounds %"{{.*}}/cl/_testrt/abinamed.eface", ptr %2, i32 0, i32 0
// CHECK-NEXT:   %116 = load ptr, ptr %115, align 8
// CHECK-NEXT:   %117 = icmp ne ptr %114, %116
// CHECK-NEXT:   br i1 %117, label %_llgo_11, label %_llgo_12
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_11:                                         ; preds = %_llgo_10
// CHECK-NEXT:   %118 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @126, i64 13 }, ptr %118, align 8
// CHECK-NEXT:   %119 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %118, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %119)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_12:                                         ; preds = %_llgo_10
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

func main() {
	e := toEface(T{})
	e2 := toEface(abi.Type{})

	println(e.typ)
	println(e.typ.PtrToThis_)
	println(e2.typ)
	println(e2.typ.PtrToThis_)

	f0 := e.typ.StructType().Fields[0]
	if f0.Typ != e.typ.PtrToThis_ {
		panic("error field 0")
	}
	if f0.Typ.Elem() != e.typ {
		panic("error field 0 elem")
	}
	f1 := e.typ.StructType().Fields[1]
	if f1.Typ != e2.typ.PtrToThis_ {
		panic("error field 1")
	}
	if f1.Typ.Elem() != e2.typ {
		panic("error field 1 elem")
	}
	f2 := e.typ.StructType().Fields[2]
	if f2.Typ != e2.typ.StructType().Fields[0].Typ {
		panic("error field 2")
	}
	f3 := e.typ.StructType().Fields[3]
	if f3.Typ.Elem() != e.typ {
		panic("error field 3")
	}
}

// CHECK-LABEL: define ptr @"{{.*}}/cl/_testrt/abinamed.toEface"(%"{{.*}}/runtime/internal/runtime.eface" %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.eface" %0, ptr %1, align 8
// CHECK-NEXT:   ret ptr %1
// CHECK-NEXT: }

func toEface(i any) *eface {
	return (*eface)(unsafe.Pointer(&i))
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

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).Align"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*Type).Align"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*Type).ArrayType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*Type).ArrayType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).Align"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*ArrayType).Align"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).ArrayType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*ArrayType).ArrayType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).ChanDir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*ArrayType).ChanDir"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).Common"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*ArrayType).Common"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).ExportedMethods"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*ArrayType).ExportedMethods"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*Method).Exported"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*Method).Exported"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*Method).Name"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*Method).Name"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*Method).PkgPath"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*Method).PkgPath"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Align"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*FuncType).Align"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).ArrayType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*FuncType).ArrayType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).ChanDir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*FuncType).ChanDir"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Common"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*FuncType).Common"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Elem"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*FuncType).Elem"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).ExportedMethods"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*FuncType).ExportedMethods"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).FieldAlign"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*FuncType).FieldAlign"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).FuncType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*FuncType).FuncType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).GcSlice"(ptr %0, ptr %1, i64 %2, i64 %3){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %4 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*FuncType).GcSlice"(ptr %1, i64 %2, i64 %3)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %4
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).HasName"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*FuncType).HasName"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).IfaceIndir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*FuncType).IfaceIndir"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).InterfaceType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*FuncType).InterfaceType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*Imethod).Exported"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*Imethod).Exported"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*Imethod).Name"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*Imethod).Name"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*Imethod).PkgPath"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*Imethod).PkgPath"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).Align"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*InterfaceType).Align"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).ArrayType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*InterfaceType).ArrayType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).ChanDir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*InterfaceType).ChanDir"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).Common"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*InterfaceType).Common"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).Elem"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*InterfaceType).Elem"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).ExportedMethods"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*InterfaceType).ExportedMethods"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).FieldAlign"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*InterfaceType).FieldAlign"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).FuncType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*InterfaceType).FuncType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).GcSlice"(ptr %0, ptr %1, i64 %2, i64 %3){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %4 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*InterfaceType).GcSlice"(ptr %1, i64 %2, i64 %3)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %4
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).HasName"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*InterfaceType).HasName"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).IfaceIndir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*InterfaceType).IfaceIndir"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).InterfaceType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*InterfaceType).InterfaceType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).IsClosure"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*InterfaceType).IsClosure"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).IsDirectIface"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*InterfaceType).IsDirectIface"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).Key"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*InterfaceType).Key"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).Kind"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*InterfaceType).Kind"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*Kind).String"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*Kind).String"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.Kind.String"(ptr %0, i64 %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.Kind.String"(i64 %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).Len"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*InterfaceType).Len"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).MapType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*InterfaceType).MapType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.memequal16"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.memequal16"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).Align"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*MapType).Align"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).ArrayType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*MapType).ArrayType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).ChanDir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*MapType).ChanDir"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).Common"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*MapType).Common"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).ExportedMethods"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*MapType).ExportedMethods"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).FieldAlign"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*MapType).FieldAlign"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).FuncType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*MapType).FuncType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).GcSlice"(ptr %0, ptr %1, i64 %2, i64 %3){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %4 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*MapType).GcSlice"(ptr %1, i64 %2, i64 %3)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %4
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).HasName"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).HasName"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).HashMightPanic"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).HashMightPanic"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).IfaceIndir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).IfaceIndir"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).IndirectElem"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).IndirectElem"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).IndirectKey"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).IndirectKey"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).InterfaceType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*MapType).InterfaceType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).IsClosure"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).IsClosure"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).IsDirectIface"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).IsDirectIface"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).Kind"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*MapType).Kind"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).Len"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*MapType).Len"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).MapType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*MapType).MapType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).NeedKeyUpdate"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).NeedKeyUpdate"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).NumMethod"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*MapType).NumMethod"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).Pointers"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).Pointers"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).ReflexiveKey"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*MapType).ReflexiveKey"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).Size"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*MapType).Size"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).String"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*MapType).String"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).StructType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*MapType).StructType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*StructField).Embedded"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*StructField).Embedded"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*StructField).Exported"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*StructField).Exported"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).Align"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*StructType).Align"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).ArrayType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*StructType).ArrayType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).ChanDir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*StructType).ChanDir"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).Common"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*StructType).Common"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).Elem"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*StructType).Elem"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).ExportedMethods"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*StructType).ExportedMethods"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).FieldAlign"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*StructType).FieldAlign"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).FuncType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*StructType).FuncType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).GcSlice"(ptr %0, ptr %1, i64 %2, i64 %3){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %4 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*StructType).GcSlice"(ptr %1, i64 %2, i64 %3)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %4
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).HasName"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*StructType).HasName"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).IfaceIndir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*StructType).IfaceIndir"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).InterfaceType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*StructType).InterfaceType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).IsClosure"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*StructType).IsClosure"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).IsDirectIface"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*StructType).IsDirectIface"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).Key"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*StructType).Key"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).Kind"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*StructType).Kind"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).Len"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*StructType).Len"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).MapType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*StructType).MapType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).NumMethod"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*StructType).NumMethod"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).Pointers"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*StructType).Pointers"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).Size"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*StructType).Size"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).String"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*StructType).String"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).StructType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*StructType).StructType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*StructType).Uncommon"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*StructType).Uncommon"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*UncommonType).ExportedMethods"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*UncommonType).ExportedMethods"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*UncommonType).Methods"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*UncommonType).Methods"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*MapType).Uncommon"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*MapType).Uncommon"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).NumMethod"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*InterfaceType).NumMethod"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).Pointers"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*InterfaceType).Pointers"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).Size"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*InterfaceType).Size"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).String"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*InterfaceType).String"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).StructType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*InterfaceType).StructType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*InterfaceType).Uncommon"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*InterfaceType).Uncommon"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).IsClosure"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*FuncType).IsClosure"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).IsDirectIface"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*FuncType).IsDirectIface"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Key"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*FuncType).Key"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Kind"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*FuncType).Kind"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Len"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*FuncType).Len"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).MapType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*FuncType).MapType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).NumMethod"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*FuncType).NumMethod"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Pointers"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*FuncType).Pointers"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Size"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*FuncType).Size"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).String"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*FuncType).String"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).StructType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*FuncType).StructType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Uncommon"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*FuncType).Uncommon"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*FuncType).Variadic"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*FuncType).Variadic"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).FieldAlign"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*ArrayType).FieldAlign"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).FuncType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*ArrayType).FuncType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).GcSlice"(ptr %0, ptr %1, i64 %2, i64 %3){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %4 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*ArrayType).GcSlice"(ptr %1, i64 %2, i64 %3)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %4
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).HasName"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*ArrayType).HasName"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).IfaceIndir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*ArrayType).IfaceIndir"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).InterfaceType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*ArrayType).InterfaceType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).IsClosure"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*ArrayType).IsClosure"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).IsDirectIface"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*ArrayType).IsDirectIface"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).Key"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*ArrayType).Key"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).Kind"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*ArrayType).Kind"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).MapType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*ArrayType).MapType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).NumMethod"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*ArrayType).NumMethod"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).Pointers"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*ArrayType).Pointers"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).Size"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*ArrayType).Size"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).String"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*ArrayType).String"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).StructType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*ArrayType).StructType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*ArrayType).Uncommon"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*ArrayType).Uncommon"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).ChanDir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*Type).ChanDir"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*Type).Common"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*Type).Common"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*Type).Elem"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*Type).Elem"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*Type).ExportedMethods"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*Type).ExportedMethods"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).FieldAlign"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*Type).FieldAlign"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*Type).FuncType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*Type).FuncType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.Slice" @"__llgo_stub.{{.*}}/runtime/abi.(*Type).GcSlice"(ptr %0, ptr %1, i64 %2, i64 %3){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %4 = tail call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/abi.(*Type).GcSlice"(ptr %1, i64 %2, i64 %3)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.Slice" %4
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).HasName"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*Type).HasName"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).IfaceIndir"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*Type).IfaceIndir"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*Type).InterfaceType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*Type).InterfaceType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).IsClosure"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*Type).IsClosure"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).IsDirectIface"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*Type).IsDirectIface"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*Type).Key"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*Type).Key"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).Kind"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*Type).Kind"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).Len"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*Type).Len"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*Type).MapType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*Type).MapType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).NumMethod"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*Type).NumMethod"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).Pointers"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i1 @"{{.*}}/runtime/abi.(*Type).Pointers"(ptr %1)
// CHECK-NEXT:   ret i1 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i64 @"__llgo_stub.{{.*}}/runtime/abi.(*Type).Size"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call i64 @"{{.*}}/runtime/abi.(*Type).Size"(ptr %1)
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce %"{{.*}}/runtime/internal/runtime.String" @"__llgo_stub.{{.*}}/runtime/abi.(*Type).String"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call %"{{.*}}/runtime/internal/runtime.String" @"{{.*}}/runtime/abi.(*Type).String"(ptr %1)
// CHECK-NEXT:   ret %"{{.*}}/runtime/internal/runtime.String" %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*Type).StructType"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*Type).StructType"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/runtime/abi.(*Type).Uncommon"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/runtime/abi.(*Type).Uncommon"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }
