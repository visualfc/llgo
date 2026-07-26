// LITTEST
package main

import (
	"github.com/goplus/lib/c"
	"github.com/goplus/lib/c/sqlite"
)

// CHECK: {{^}}@0 = private unnamed_addr constant [20 x i8] c"==> Error: (%d) %s\0A\00", align 1{{$}}
// CHECK: {{^}}@1 = private unnamed_addr constant [9 x i8] c":memory:\00", align 1{{$}}

// CHECK-LABEL: define void @"{{.*}}/cl/_testlibc/sqlite.check"(i32 %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = icmp ne i32 %0, 0
// CHECK-NEXT:   br i1 %1, label %_llgo_1, label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %2 = call ptr @sqlite3_errstr(i32 %0)
// CHECK-NEXT:   %3 = call i32 (ptr, ...) @printf(ptr @0, i32 %0, ptr %2)
// CHECK-NEXT:   call void @exit(i32 1)
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

func check(err sqlite.Errno) {
	if err != sqlite.OK {
		c.Printf(c.Str("==> Error: (%d) %s\n"), err, err.Errstr())
		c.Exit(1)
	}
}

func main() {
	db, err := sqlite.OpenV2(c.Str(":memory:"), sqlite.OpenReadWrite|sqlite.OpenMemory, nil)
	check(err)

	db.Close()
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testlibc/sqlite.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testlibc/sqlite.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testlibc/sqlite.init$guard", align 1
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testlibc/sqlite.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call { ptr, i32 } @"github.com/goplus/lib/c/sqlite.OpenV2"(ptr @1, i32 130, ptr null)
// CHECK-NEXT:   %1 = extractvalue { ptr, i32 } %0, 0
// CHECK-NEXT:   %2 = extractvalue { ptr, i32 } %0, 1
// CHECK-NEXT:   call void @"{{.*}}/cl/_testlibc/sqlite.check"(i32 %2)
// CHECK-NEXT:   %3 = call i32 @sqlite3_close(ptr %1)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
