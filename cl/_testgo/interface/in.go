// LITTEST
package main

import (
	"github.com/goplus/llgo/cl/_testdata/foo"
)

// CHECK: {{^}}@12 = private unnamed_addr constant [2 x i8] c"OK", align 1{{$}}
// CHECK: {{^}}@13 = private unnamed_addr constant [4 x i8] c"FAIL", align 1{{$}}

type Game1 struct {
	*foo.Game
}

type Game2 struct {
}

func (p *Game2) initGame() {
}

func main() {
	var g1 any = &Game1{&foo.Game{}}

	var g2 any = &Game2{}

	v1, ok := g1.(foo.Gamer)

	println("OK", v1, ok)

	if ok {
		v1.Load()
	}

	v2, ok := g2.(foo.Gamer)

	println("FAIL", v2, ok)
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/interface.Game1.Load"(%"{{.*}}/cl/_testgo/interface.Game1" %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = alloca %"{{.*}}/cl/_testgo/interface.Game1", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %1, i8 0, i64 8, i1 false)
// CHECK-NEXT:   store %"{{.*}}/cl/_testgo/interface.Game1" %0, ptr %1, align 8
// CHECK-NEXT:   %2 = getelementptr inbounds %"{{.*}}/cl/_testgo/interface.Game1", ptr %1, i32 0, i32 0
// CHECK-NEXT:   %3 = load ptr, ptr %2, align 8
// CHECK-NEXT:   call void @"{{.*}}/cl/_testdata/foo.(*Game).Load"(ptr %3)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/interface.Game1.initGame"(%"{{.*}}/cl/_testgo/interface.Game1" %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = alloca %"{{.*}}/cl/_testgo/interface.Game1", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %1, i8 0, i64 8, i1 false)
// CHECK-NEXT:   store %"{{.*}}/cl/_testgo/interface.Game1" %0, ptr %1, align 8
// CHECK-NEXT:   %2 = getelementptr inbounds %"{{.*}}/cl/_testgo/interface.Game1", ptr %1, i32 0, i32 0
// CHECK-NEXT:   %3 = load ptr, ptr %2, align 8
// CHECK-NEXT:   call void @"{{.*}}/cl/_testdata/foo.(*Game).initGame"(ptr %3)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/interface.(*Game1).Load"(ptr %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = getelementptr inbounds %"{{.*}}/cl/_testgo/interface.Game1", ptr %0, i32 0, i32 0
// CHECK-NEXT:   %2 = load ptr, ptr %1, align 8
// CHECK-NEXT:   call void @"{{.*}}/cl/_testdata/foo.(*Game).Load"(ptr %2)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/interface.(*Game1).initGame"(ptr %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = getelementptr inbounds %"{{.*}}/cl/_testgo/interface.Game1", ptr %0, i32 0, i32 0
// CHECK-NEXT:   %2 = load ptr, ptr %1, align 8
// CHECK-NEXT:   call void @"{{.*}}/cl/_testdata/foo.(*Game).initGame"(ptr %2)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/interface.(*Game2).initGame"(ptr %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/interface.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testgo/interface.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testgo/interface.init$guard", align 1
// CHECK-NEXT:   call void @"{{.*}}/cl/_testdata/foo.init"()
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/interface.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 8)
// CHECK-NEXT:   %1 = getelementptr inbounds %"{{.*}}/cl/_testgo/interface.Game1", ptr %0, i32 0, i32 0
// CHECK-NEXT:   store ptr @"__llgo.moduleZeroSizedAlloc$", ptr %1, align 8
// CHECK-NEXT:   %2 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"*_llgo_{{.*}}/cl/_testgo/interface.Game1", ptr undef }, ptr %0, 1
// CHECK-NEXT:   %3 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %2, 0
// CHECK-NEXT:   %4 = call i1 @"{{.*}}/runtime/internal/runtime.Implements"(ptr @"_llgo_{{.*}}/cl/_testdata/foo.Gamer", ptr %3)
// CHECK-NEXT:   br i1 %4, label %_llgo_3, label %_llgo_4
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_5
// CHECK-NEXT:   %5 = call ptr @"{{.*}}/runtime/internal/runtime.IfacePtrData"(%"{{.*}}/runtime/internal/runtime.iface" %21)
// CHECK-NEXT:   %6 = extractvalue %"{{.*}}/runtime/internal/runtime.iface" %21, 0
// CHECK-NEXT:   %7 = getelementptr ptr, ptr %6, i64 3
// CHECK-NEXT:   %8 = load ptr, ptr %7, align 8
// CHECK-NEXT:   %9 = insertvalue { ptr, ptr } undef, ptr %8, 0
// CHECK-NEXT:   %10 = insertvalue { ptr, ptr } %9, ptr %5, 1
// CHECK-NEXT:   %11 = extractvalue { ptr, ptr } %10, 1
// CHECK-NEXT:   %12 = extractvalue { ptr, ptr } %10, 0
// CHECK-NEXT:   call void %12(ptr %11)
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_5
// CHECK-NEXT:   %13 = call i1 @"{{.*}}/runtime/internal/runtime.Implements"(ptr @"_llgo_{{.*}}/cl/_testdata/foo.Gamer", ptr @"*_llgo_{{.*}}/cl/_testgo/interface.Game2")
// CHECK-NEXT:   br i1 %13, label %_llgo_6, label %_llgo_7
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %14 = extractvalue %"{{.*}}/runtime/internal/runtime.eface" %2, 1
// CHECK-NEXT:   %15 = call ptr @"{{.*}}/runtime/internal/runtime.NewItab"(ptr @"{{.*}}/cl/_testdata/foo.iface$sO8a1LvuUsjXwiwaC6sR9-L4DiYgiOnZi7iosyShJXg", ptr %3)
// CHECK-NEXT:   %16 = insertvalue %"{{.*}}/runtime/internal/runtime.iface" undef, ptr %15, 0
// CHECK-NEXT:   %17 = insertvalue %"{{.*}}/runtime/internal/runtime.iface" %16, ptr %14, 1
// CHECK-NEXT:   %18 = insertvalue { %"{{.*}}/runtime/internal/runtime.iface", i1 } undef, %"{{.*}}/runtime/internal/runtime.iface" %17, 0
// CHECK-NEXT:   %19 = insertvalue { %"{{.*}}/runtime/internal/runtime.iface", i1 } %18, i1 true, 1
// CHECK-NEXT:   br label %_llgo_5
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_4:                                          ; preds = %_llgo_0
// CHECK-NEXT:   br label %_llgo_5
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_5:                                          ; preds = %_llgo_4, %_llgo_3
// CHECK-NEXT:   %20 = phi { %"{{.*}}/runtime/internal/runtime.iface", i1 } [ %19, %_llgo_3 ], [ zeroinitializer, %_llgo_4 ]
// CHECK-NEXT:   %21 = extractvalue { %"{{.*}}/runtime/internal/runtime.iface", i1 } %20, 0
// CHECK-NEXT:   %22 = extractvalue { %"{{.*}}/runtime/internal/runtime.iface", i1 } %20, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" { ptr @12, i64 2 })
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintIface"(%"{{.*}}/runtime/internal/runtime.iface" %21)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintBool"(i1 %22)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br i1 %22, label %_llgo_1, label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_6:                                          ; preds = %_llgo_2
// CHECK-NEXT:   %23 = call ptr @"{{.*}}/runtime/internal/runtime.NewItab"(ptr @"{{.*}}/cl/_testdata/foo.iface$sO8a1LvuUsjXwiwaC6sR9-L4DiYgiOnZi7iosyShJXg", ptr @"*_llgo_{{.*}}/cl/_testgo/interface.Game2")
// CHECK-NEXT:   %24 = insertvalue %"{{.*}}/runtime/internal/runtime.iface" undef, ptr %23, 0
// CHECK-NEXT:   %25 = insertvalue %"{{.*}}/runtime/internal/runtime.iface" %24, ptr @"__llgo.moduleZeroSizedAlloc$", 1
// CHECK-NEXT:   %26 = insertvalue { %"{{.*}}/runtime/internal/runtime.iface", i1 } undef, %"{{.*}}/runtime/internal/runtime.iface" %25, 0
// CHECK-NEXT:   %27 = insertvalue { %"{{.*}}/runtime/internal/runtime.iface", i1 } %26, i1 true, 1
// CHECK-NEXT:   br label %_llgo_8
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_7:                                          ; preds = %_llgo_2
// CHECK-NEXT:   br label %_llgo_8
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_8:                                          ; preds = %_llgo_7, %_llgo_6
// CHECK-NEXT:   %28 = phi { %"{{.*}}/runtime/internal/runtime.iface", i1 } [ %27, %_llgo_6 ], [ zeroinitializer, %_llgo_7 ]
// CHECK-NEXT:   %29 = extractvalue { %"{{.*}}/runtime/internal/runtime.iface", i1 } %28, 0
// CHECK-NEXT:   %30 = extractvalue { %"{{.*}}/runtime/internal/runtime.iface", i1 } %28, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" { ptr @13, i64 4 })
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintIface"(%"{{.*}}/runtime/internal/runtime.iface" %29)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintBool"(i1 %30)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.memequal0"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.memequal0"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce void @"__llgo_stub.{{.*}}/cl/_testdata/foo.(*Game).Load"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   tail call void @"{{.*}}/cl/_testdata/foo.(*Game).Load"(ptr %1)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce void @"__llgo_stub.{{.*}}/cl/_testdata/foo.(*Game).initGame"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   tail call void @"{{.*}}/cl/_testdata/foo.(*Game).initGame"(ptr %1)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce void @"__llgo_stub.{{.*}}/cl/_testgo/interface.Game1.Load"(ptr %0, %"{{.*}}/cl/_testgo/interface.Game1" %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   tail call void @"{{.*}}/cl/_testgo/interface.Game1.Load"(%"{{.*}}/cl/_testgo/interface.Game1" %1)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce void @"__llgo_stub.{{.*}}/cl/_testgo/interface.Game1.initGame"(ptr %0, %"{{.*}}/cl/_testgo/interface.Game1" %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   tail call void @"{{.*}}/cl/_testgo/interface.Game1.initGame"(%"{{.*}}/cl/_testgo/interface.Game1" %1)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce void @"__llgo_stub.{{.*}}/cl/_testgo/interface.(*Game1).Load"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   tail call void @"{{.*}}/cl/_testgo/interface.(*Game1).Load"(ptr %1)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce void @"__llgo_stub.{{.*}}/cl/_testgo/interface.(*Game1).initGame"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   tail call void @"{{.*}}/cl/_testgo/interface.(*Game1).initGame"(ptr %1)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce void @"__llgo_stub.{{.*}}/cl/_testgo/interface.(*Game2).initGame"(ptr %0, ptr %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   tail call void @"{{.*}}/cl/_testgo/interface.(*Game2).initGame"(ptr %1)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.interequal"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.interequal"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }
