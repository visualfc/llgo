// LITTEST
package main

// CHECK-LABEL: define i64 @"{{.*}}/cl/_testgo/makemaphint.fromInt32"(i32 %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = sext i32 %0 to i64
// CHECK-NEXT:   %2 = call ptr @"{{.*}}/runtime/internal/runtime.MakeMap"(ptr @"map[_llgo_string]_llgo_int", i64 %1)
// CHECK-NEXT:   %3 = call i64 @"{{.*}}/runtime/internal/runtime.MapLen"(ptr %2)
// CHECK-NEXT:   ret i64 %3
// CHECK-NEXT: }

func fromInt32(n int32) int {
	return len(make(map[string]int, n))
}

// CHECK-LABEL: define i64 @"{{.*}}/cl/_testgo/makemaphint.fromUint32"(i32 %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = zext i32 %0 to i64
// CHECK-NEXT:   %2 = call ptr @"{{.*}}/runtime/internal/runtime.MakeMap"(ptr @"map[_llgo_string]_llgo_int", i64 %1)
// CHECK-NEXT:   %3 = call i64 @"{{.*}}/runtime/internal/runtime.MapLen"(ptr %2)
// CHECK-NEXT:   ret i64 %3
// CHECK-NEXT: }

func fromUint32(n uint32) int {
	return len(make(map[string]int, n))
}

func main() {
	println(fromUint32(2))
	println(fromInt32(3))
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/makemaphint.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testgo/makemaphint.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testgo/makemaphint.init$guard", align 1
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/makemaphint.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call i64 @"{{.*}}/cl/_testgo/makemaphint.fromUint32"(i32 2)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintInt"(i64 %0)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   %1 = call i64 @"{{.*}}/cl/_testgo/makemaphint.fromInt32"(i32 3)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintInt"(i64 %1)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

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
