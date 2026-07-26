// LITTEST
package main

import "github.com/goplus/llgo/cl/_testdata/foo"

// CHECK: {{^}}@11 = private unnamed_addr constant [6 x i8] c"notOk:", align 1{{$}}

type bar struct {
	pb *byte
	f  float32
}

// CHECK-LABEL: define { %"{{.*}}/cl/_testdata/foo.Foo", i1 } @"{{.*}}/cl/_testgo/struczero.Bar"(%"{{.*}}/runtime/internal/runtime.eface" %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %0, 0
// CHECK-NEXT:   %2 = icmp eq ptr %1, @"_llgo_{{.*}}/cl/_testdata/foo.Foo"
// CHECK-NEXT:   br i1 %2, label %_llgo_1, label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %3 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %0, 1
// CHECK-NEXT:   %4 = load %"{{.*}}/cl/_testdata/foo.Foo", ptr %3, align 8
// CHECK-NEXT:   %5 = insertvalue { %"{{.*}}/cl/_testdata/foo.Foo", i1 } undef, %"{{.*}}/cl/_testdata/foo.Foo" %4, 0
// CHECK-NEXT:   %6 = insertvalue { %"{{.*}}/cl/_testdata/foo.Foo", i1 } %5, i1 true, 1
// CHECK-NEXT:   br label %_llgo_3
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_0
// CHECK-NEXT:   br label %_llgo_3
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_2, %_llgo_1
// CHECK-NEXT:   %7 = phi { %"{{.*}}/cl/_testdata/foo.Foo", i1 } [ %6, %_llgo_1 ], [ zeroinitializer, %_llgo_2 ]
// CHECK-NEXT:   %8 = extractvalue { %"{{.*}}/cl/_testdata/foo.Foo", i1 } %7, 0
// CHECK-NEXT:   %9 = extractvalue { %"{{.*}}/cl/_testdata/foo.Foo", i1 } %7, 1
// CHECK-NEXT:   %10 = insertvalue { %"{{.*}}/cl/_testdata/foo.Foo", i1 } undef, %"{{.*}}/cl/_testdata/foo.Foo" %8, 0
// CHECK-NEXT:   %11 = insertvalue { %"{{.*}}/cl/_testdata/foo.Foo", i1 } %10, i1 %9, 1
// CHECK-NEXT:   ret { %"{{.*}}/cl/_testdata/foo.Foo", i1 } %11
// CHECK-NEXT: }

func Bar(v any) (ret foo.Foo, ok bool) {
	ret, ok = v.(foo.Foo)
	return
}

// CHECK-LABEL: define { %"{{.*}}/cl/_testgo/struczero.bar", i1 } @"{{.*}}/cl/_testgo/struczero.Foo"(%"{{.*}}/runtime/internal/runtime.eface" %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %0, 0
// CHECK-NEXT:   %2 = icmp eq ptr %1, @"_llgo_{{.*}}/cl/_testgo/struczero.bar"
// CHECK-NEXT:   br i1 %2, label %_llgo_1, label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %3 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %0, 1
// CHECK-NEXT:   %4 = load %"{{.*}}/cl/_testgo/struczero.bar", ptr %3, align 8
// CHECK-NEXT:   %5 = insertvalue { %"{{.*}}/cl/_testgo/struczero.bar", i1 } undef, %"{{.*}}/cl/_testgo/struczero.bar" %4, 0
// CHECK-NEXT:   %6 = insertvalue { %"{{.*}}/cl/_testgo/struczero.bar", i1 } %5, i1 true, 1
// CHECK-NEXT:   br label %_llgo_3
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_0
// CHECK-NEXT:   br label %_llgo_3
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_2, %_llgo_1
// CHECK-NEXT:   %7 = phi { %"{{.*}}/cl/_testgo/struczero.bar", i1 } [ %6, %_llgo_1 ], [ zeroinitializer, %_llgo_2 ]
// CHECK-NEXT:   %8 = extractvalue { %"{{.*}}/cl/_testgo/struczero.bar", i1 } %7, 0
// CHECK-NEXT:   %9 = extractvalue { %"{{.*}}/cl/_testgo/struczero.bar", i1 } %7, 1
// CHECK-NEXT:   %10 = insertvalue { %"{{.*}}/cl/_testgo/struczero.bar", i1 } undef, %"{{.*}}/cl/_testgo/struczero.bar" %8, 0
// CHECK-NEXT:   %11 = insertvalue { %"{{.*}}/cl/_testgo/struczero.bar", i1 } %10, i1 %9, 1
// CHECK-NEXT:   ret { %"{{.*}}/cl/_testgo/struczero.bar", i1 } %11
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/struczero.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testgo/struczero.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testgo/struczero.init$guard", align 1
// CHECK-NEXT:   call void @"{{.*}}/cl/_testdata/foo.init"()
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

func Foo(v any) (ret bar, ok bool) {
	ret, ok = v.(bar)
	return
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/struczero.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = alloca %"{{.*}}/cl/_testgo/struczero.bar", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %0, i8 0, i64 16, i1 false)
// CHECK-NEXT:   %1 = call { %"{{.*}}/cl/_testgo/struczero.bar", i1 } @"{{.*}}/cl/_testgo/struczero.Foo"(%"{{.*}}/runtime/internal/runtime.eface" zeroinitializer)
// CHECK-NEXT:   %2 = extractvalue { %"{{.*}}/cl/_testgo/struczero.bar", i1 } %1, 0
// CHECK-NEXT:   store %"{{.*}}/cl/_testgo/struczero.bar" %2, ptr %0, align 8
// CHECK-NEXT:   %3 = extractvalue { %"{{.*}}/cl/_testgo/struczero.bar", i1 } %1, 1
// CHECK-NEXT:   %4 = getelementptr inbounds %"{{.*}}/cl/_testgo/struczero.bar", ptr %0, i32 0, i32 0
// CHECK-NEXT:   %5 = load ptr, ptr %4, align 8
// CHECK-NEXT:   %6 = getelementptr inbounds %"{{.*}}/cl/_testgo/struczero.bar", ptr %0, i32 0, i32 1
// CHECK-NEXT:   %7 = load float, ptr %6, align 4
// CHECK-NEXT:   %8 = xor i1 %3, true
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintPointer"(ptr %5)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   %9 = fpext float %7 to double
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintFloat"(double %9)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" { ptr @11, i64 6 })
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintBool"(i1 %8)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   %10 = alloca %"{{.*}}/cl/_testdata/foo.Foo", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %10, i8 0, i64 16, i1 false)
// CHECK-NEXT:   %11 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/cl/_testdata/foo.Foo" zeroinitializer, ptr %11, align 8
// CHECK-NEXT:   %12 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_{{.*}}/cl/_testdata/foo.Foo", ptr undef }, ptr %11, 1
// CHECK-NEXT:   %13 = call { %"{{.*}}/cl/_testdata/foo.Foo", i1 } @"{{.*}}/cl/_testgo/struczero.Bar"(%"{{.*}}/runtime/internal/runtime.eface" %12)
// CHECK-NEXT:   %14 = extractvalue { %"{{.*}}/cl/_testdata/foo.Foo", i1 } %13, 0
// CHECK-NEXT:   store %"{{.*}}/cl/_testdata/foo.Foo" %14, ptr %10, align 8
// CHECK-NEXT:   %15 = extractvalue { %"{{.*}}/cl/_testdata/foo.Foo", i1 } %13, 1
// CHECK-NEXT:   %16 = load %"{{.*}}/cl/_testdata/foo.Foo", ptr %10, align 8
// CHECK-NEXT:   %17 = call ptr @"{{.*}}/cl/_testdata/foo.Foo.Pb"(%"{{.*}}/cl/_testdata/foo.Foo" %16)
// CHECK-NEXT:   %18 = getelementptr inbounds %"{{.*}}/cl/_testdata/foo.Foo", ptr %10, i32 0, i32 1
// CHECK-NEXT:   %19 = load float, ptr %18, align 4
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintPointer"(ptr %17)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   %20 = fpext float %19 to double
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintFloat"(double %20)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintBool"(i1 %15)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

func main() {
	ret, ok := Foo(nil)
	println(ret.pb, ret.f, "notOk:", !ok)

	ret2, ok2 := Bar(foo.Foo{})
	println(ret2.Pb(), ret2.F, ok2)
}

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/cl/_testdata/foo.(*Foo).Pb"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/cl/_testdata/foo.(*Foo).Pb"(ptr %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.memequal8"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.memequal8"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.f32equal"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.f32equal"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce ptr @"__llgo_stub.{{.*}}/cl/_testdata/foo.Foo.Pb"(ptr %0, %"{{.*}}/cl/_testdata/foo.Foo" %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = tail call ptr @"{{.*}}/cl/_testdata/foo.Foo.Pb"(%"{{.*}}/cl/_testdata/foo.Foo" %1)
// CHECK-NEXT:   ret ptr %2
// CHECK-NEXT: }
