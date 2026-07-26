// LITTEST
package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/cpp/llvm"
)

// CHECK: {{^}}@0 = private unnamed_addr constant [29 x i8] c"__ZNK9INIReader10ParseErrorEv", align 1{{$}}
// CHECK: {{^}}@1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1{{$}}
// CHECK: {{^}}@2 = private unnamed_addr constant [18 x i8] c"Failed to demangle", align 1{{$}}

func main() {
	mangledName := "__ZNK9INIReader10ParseErrorEv"
	if name := llvm.ItaniumDemangle(mangledName, true); name != nil {
		c.Printf(c.Str("%s\n"), name)
	} else {
		println("Failed to demangle")
	}
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testlibc/demangle.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testlibc/demangle.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testlibc/demangle.init$guard", align 1
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testlibc/demangle.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @_ZN4llvm15itaniumDemangleENSt3__117basic_string_viewIcNS0_11char_traitsIcEEEEb(%"{{.*}}/runtime/internal/runtime.String" { ptr @0, i64 29 }, i1 true)
// CHECK-NEXT:   %1 = icmp ne ptr %0, null
// CHECK-NEXT:   br i1 %1, label %_llgo_1, label %_llgo_3
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %2 = call i32 (ptr, ...) @printf(ptr @1, ptr %0)
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_3, %_llgo_1
// CHECK-NEXT:   ret void
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_3:                                          ; preds = %_llgo_0
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintString"(%"{{.*}}/runtime/internal/runtime.String" { ptr @2, i64 18 })
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   br label %_llgo_2
// CHECK-NEXT: }
