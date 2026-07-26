// LITTEST
package main

// CHECK: {{^}}@0 = private unnamed_addr constant [6 x i8] c"failed", align 1{{$}}
// CHECK: {{^}}@2 = private unnamed_addr constant [5 x i8] c"hello", align 1{{$}}
// CHECK: {{^}}@4 = private unnamed_addr constant [2 x i8] c"ok", align 1{{$}}

func test() {}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.assert"(i1 %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   %1 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @0, i64 6 }, ptr %1, align 8
// CHECK-NEXT:   %2 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %1, 1
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.Panic"(%"{{.*}}/runtime/internal/runtime.eface" %2)
// CHECK-NEXT:   unreachable
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

func assert(cond bool) {
	if !cond {
		panic("failed")
	}
}

// func
func init() {
	fn1 := test
	fn2 := func(i, j int) int { return i + j }
	var n int
	fn3 := func() { println(n) }
	var fn4 func() int
	assert(test != nil)
	assert(nil != test)
	assert(fn1 != nil)
	assert(nil != fn1)
	assert(fn2 != nil)
	assert(nil != fn2)
	assert(fn3 != nil)
	assert(nil != fn3)
	assert(fn4 == nil)
	assert(nil == fn4)
}

// array
func init() {
	assert([0]float64{} == [0]float64{})
	ar1 := [...]int{1, 2, 3}
	ar2 := [...]int{1, 2, 3}
	assert(ar1 == ar2)
	ar2[1] = 1
	assert(ar1 != ar2)
}

type T struct {
	X int
	Y int
	Z string
	V any
}

type N struct{}

// struct
func init() {
	var n1, n2 N
	var t1, t2 T
	x := T{10, 20, "hello", 1}
	y := T{10, 20, "hello", 1}
	z := T{10, 20, "hello", "ok"}
	assert(n1 == n2)
	assert(t1 == t2)
	assert(x == y)
	assert(x != z)
	assert(y != z)
}

// slice
func init() {
	var a []int
	var b = []int{1, 2, 3}
	c := make([]int, 2)
	d := make([]int, 0, 2)
	assert(a == nil)
	assert(b != nil)
	assert(c != nil)
	assert(d != nil)
	b = nil
	assert(b == nil)
}

// iface
func init() {
	var a any = 100
	var b any = struct{}{}
	var c any = T{10, 20, "hello", 1}
	x := T{10, 20, "hello", 1}
	y := T{10, 20, "hello", "ok"}
	assert(a == 100)
	assert(b == struct{}{})
	assert(b != N{})
	assert(c == x)
	assert(c != y)
}

// chan
func init() {
	a := make(chan int)
	b := make(chan int)
	assert(a == a)
	assert(a != b)
	assert(a != nil)
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testgo/equal.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testgo/equal.init$guard", align 1
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.init#1"()
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.init#2"()
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.init#3"()
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.init#4"()
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.init#5"()
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.init#6"()
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.init#7"()
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// map
func init() {
	m1 := make(map[int]string)
	var m2 map[int]string
	assert(m1 != nil)
	assert(m2 == nil)
}

func main() {
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.init#1"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 8)
// CHECK-NEXT:   %1 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 8)
// CHECK-NEXT:   %2 = getelementptr inbounds { ptr }, ptr %1, i32 0, i32 0
// CHECK-NEXT:   store ptr %0, ptr %2, align 8
// CHECK-NEXT:   %3 = insertvalue { ptr, ptr } { ptr @"{{.*}}/cl/_testgo/equal.init#1$2", ptr undef }, ptr %1, 1
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   %4 = extractvalue { ptr, ptr } %3, 0
// CHECK-NEXT:   %5 = icmp ne ptr %4, null
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %5)
// CHECK-NEXT:   %6 = extractvalue { ptr, ptr } %3, 0
// CHECK-NEXT:   %7 = icmp ne ptr null, %6
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %7)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define i64 @"{{.*}}/cl/_testgo/equal.init#1$1"(i64 %0, i64 %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = add i64 %0, %1
// CHECK-NEXT:   ret i64 %2
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.init#1$2"(ptr %0){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %1 = load { ptr }, ptr %0, align 8
// CHECK-NEXT:   %2 = extractvalue { ptr } %1, 0
// CHECK-NEXT:   %3 = load i64, ptr %2, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintInt"(i64 %3)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.init#2"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   %0 = alloca [3 x i64], align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %0, i8 0, i64 24, i1 false)
// CHECK-NEXT:   %1 = getelementptr inbounds i64, ptr %0, i64 0
// CHECK-NEXT:   %2 = getelementptr inbounds i64, ptr %0, i64 1
// CHECK-NEXT:   %3 = getelementptr inbounds i64, ptr %0, i64 2
// CHECK-NEXT:   store i64 1, ptr %1, align 8
// CHECK-NEXT:   store i64 2, ptr %2, align 8
// CHECK-NEXT:   store i64 3, ptr %3, align 8
// CHECK-NEXT:   %4 = alloca [3 x i64], align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %4, i8 0, i64 24, i1 false)
// CHECK-NEXT:   %5 = getelementptr inbounds i64, ptr %4, i64 0
// CHECK-NEXT:   %6 = getelementptr inbounds i64, ptr %4, i64 1
// CHECK-NEXT:   %7 = getelementptr inbounds i64, ptr %4, i64 2
// CHECK-NEXT:   store i64 1, ptr %5, align 8
// CHECK-NEXT:   store i64 2, ptr %6, align 8
// CHECK-NEXT:   store i64 3, ptr %7, align 8
// CHECK-NEXT:   %8 = load [3 x i64], ptr %0, align 8
// CHECK-NEXT:   %9 = load [3 x i64], ptr %4, align 8
// CHECK-NEXT:   %10 = extractvalue [3 x i64] %8, 0
// CHECK-NEXT:   %11 = extractvalue [3 x i64] %9, 0
// CHECK-NEXT:   %12 = icmp eq i64 %10, %11
// CHECK-NEXT:   %13 = and i1 true, %12
// CHECK-NEXT:   %14 = extractvalue [3 x i64] %8, 1
// CHECK-NEXT:   %15 = extractvalue [3 x i64] %9, 1
// CHECK-NEXT:   %16 = icmp eq i64 %14, %15
// CHECK-NEXT:   %17 = and i1 %13, %16
// CHECK-NEXT:   %18 = extractvalue [3 x i64] %8, 2
// CHECK-NEXT:   %19 = extractvalue [3 x i64] %9, 2
// CHECK-NEXT:   %20 = icmp eq i64 %18, %19
// CHECK-NEXT:   %21 = and i1 %17, %20
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %21)
// CHECK-NEXT:   %22 = getelementptr inbounds i64, ptr %4, i64 1
// CHECK-NEXT:   store i64 1, ptr %22, align 8
// CHECK-NEXT:   %23 = load [3 x i64], ptr %0, align 8
// CHECK-NEXT:   %24 = load [3 x i64], ptr %4, align 8
// CHECK-NEXT:   %25 = extractvalue [3 x i64] %23, 0
// CHECK-NEXT:   %26 = extractvalue [3 x i64] %24, 0
// CHECK-NEXT:   %27 = icmp eq i64 %25, %26
// CHECK-NEXT:   %28 = and i1 true, %27
// CHECK-NEXT:   %29 = extractvalue [3 x i64] %23, 1
// CHECK-NEXT:   %30 = extractvalue [3 x i64] %24, 1
// CHECK-NEXT:   %31 = icmp eq i64 %29, %30
// CHECK-NEXT:   %32 = and i1 %28, %31
// CHECK-NEXT:   %33 = extractvalue [3 x i64] %23, 2
// CHECK-NEXT:   %34 = extractvalue [3 x i64] %24, 2
// CHECK-NEXT:   %35 = icmp eq i64 %33, %34
// CHECK-NEXT:   %36 = and i1 %32, %35
// CHECK-NEXT:   %37 = xor i1 %36, true
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %37)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.init#3"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = alloca %"{{.*}}/cl/_testgo/equal.T", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %0, i8 0, i64 48, i1 false)
// CHECK-NEXT:   %1 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %0, i32 0, i32 0
// CHECK-NEXT:   %2 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %0, i32 0, i32 1
// CHECK-NEXT:   %3 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %0, i32 0, i32 2
// CHECK-NEXT:   %4 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %0, i32 0, i32 3
// CHECK-NEXT:   store i64 10, ptr %1, align 8
// CHECK-NEXT:   store i64 20, ptr %2, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @2, i64 5 }, ptr %3, align 8
// CHECK-NEXT:   %5 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 8)
// CHECK-NEXT:   store i64 1, ptr %5, align 8
// CHECK-NEXT:   %6 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_int, ptr undef }, ptr %5, 1
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.eface" %6, ptr %4, align 8
// CHECK-NEXT:   %7 = alloca %"{{.*}}/cl/_testgo/equal.T", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %7, i8 0, i64 48, i1 false)
// CHECK-NEXT:   %8 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %7, i32 0, i32 0
// CHECK-NEXT:   %9 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %7, i32 0, i32 1
// CHECK-NEXT:   %10 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %7, i32 0, i32 2
// CHECK-NEXT:   %11 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %7, i32 0, i32 3
// CHECK-NEXT:   store i64 10, ptr %8, align 8
// CHECK-NEXT:   store i64 20, ptr %9, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @2, i64 5 }, ptr %10, align 8
// CHECK-NEXT:   %12 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 8)
// CHECK-NEXT:   store i64 1, ptr %12, align 8
// CHECK-NEXT:   %13 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_int, ptr undef }, ptr %12, 1
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.eface" %13, ptr %11, align 8
// CHECK-NEXT:   %14 = alloca %"{{.*}}/cl/_testgo/equal.T", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %14, i8 0, i64 48, i1 false)
// CHECK-NEXT:   %15 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %14, i32 0, i32 0
// CHECK-NEXT:   %16 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %14, i32 0, i32 1
// CHECK-NEXT:   %17 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %14, i32 0, i32 2
// CHECK-NEXT:   %18 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %14, i32 0, i32 3
// CHECK-NEXT:   store i64 10, ptr %15, align 8
// CHECK-NEXT:   store i64 20, ptr %16, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @2, i64 5 }, ptr %17, align 8
// CHECK-NEXT:   %19 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @4, i64 2 }, ptr %19, align 8
// CHECK-NEXT:   %20 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %19, 1
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.eface" %20, ptr %18, align 8
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   %21 = call i1 @"{{.*}}/runtime/internal/runtime.StringEqual"(%"{{.*}}/runtime/internal/runtime.String" zeroinitializer, %"{{.*}}/runtime/internal/runtime.String" zeroinitializer)
// CHECK-NEXT:   %22 = and i1 true, %21
// CHECK-NEXT:   %23 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" zeroinitializer, %"{{.*}}/runtime/internal/runtime.eface" zeroinitializer)
// CHECK-NEXT:   %24 = and i1 %22, %23
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %24)
// CHECK-NEXT:   %25 = load %"{{.*}}/cl/_testgo/equal.T", ptr %0, align 8
// CHECK-NEXT:   %26 = load %"{{.*}}/cl/_testgo/equal.T", ptr %7, align 8
// CHECK-NEXT:   %27 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %25, 0
// CHECK-NEXT:   %28 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %26, 0
// CHECK-NEXT:   %29 = icmp eq i64 %27, %28
// CHECK-NEXT:   %30 = and i1 true, %29
// CHECK-NEXT:   %31 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %25, 1
// CHECK-NEXT:   %32 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %26, 1
// CHECK-NEXT:   %33 = icmp eq i64 %31, %32
// CHECK-NEXT:   %34 = and i1 %30, %33
// CHECK-NEXT:   %35 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %25, 2
// CHECK-NEXT:   %36 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %26, 2
// CHECK-NEXT:   %37 = call i1 @"{{.*}}/runtime/internal/runtime.StringEqual"(%"{{.*}}/runtime/internal/runtime.String" %35, %"{{.*}}/runtime/internal/runtime.String" %36)
// CHECK-NEXT:   %38 = and i1 %34, %37
// CHECK-NEXT:   %39 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %25, 3
// CHECK-NEXT:   %40 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %26, 3
// CHECK-NEXT:   %41 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" %39, %"{{.*}}/runtime/internal/runtime.eface" %40)
// CHECK-NEXT:   %42 = and i1 %38, %41
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %42)
// CHECK-NEXT:   %43 = load %"{{.*}}/cl/_testgo/equal.T", ptr %0, align 8
// CHECK-NEXT:   %44 = load %"{{.*}}/cl/_testgo/equal.T", ptr %14, align 8
// CHECK-NEXT:   %45 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %43, 0
// CHECK-NEXT:   %46 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %44, 0
// CHECK-NEXT:   %47 = icmp eq i64 %45, %46
// CHECK-NEXT:   %48 = and i1 true, %47
// CHECK-NEXT:   %49 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %43, 1
// CHECK-NEXT:   %50 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %44, 1
// CHECK-NEXT:   %51 = icmp eq i64 %49, %50
// CHECK-NEXT:   %52 = and i1 %48, %51
// CHECK-NEXT:   %53 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %43, 2
// CHECK-NEXT:   %54 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %44, 2
// CHECK-NEXT:   %55 = call i1 @"{{.*}}/runtime/internal/runtime.StringEqual"(%"{{.*}}/runtime/internal/runtime.String" %53, %"{{.*}}/runtime/internal/runtime.String" %54)
// CHECK-NEXT:   %56 = and i1 %52, %55
// CHECK-NEXT:   %57 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %43, 3
// CHECK-NEXT:   %58 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %44, 3
// CHECK-NEXT:   %59 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" %57, %"{{.*}}/runtime/internal/runtime.eface" %58)
// CHECK-NEXT:   %60 = and i1 %56, %59
// CHECK-NEXT:   %61 = xor i1 %60, true
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %61)
// CHECK-NEXT:   %62 = load %"{{.*}}/cl/_testgo/equal.T", ptr %7, align 8
// CHECK-NEXT:   %63 = load %"{{.*}}/cl/_testgo/equal.T", ptr %14, align 8
// CHECK-NEXT:   %64 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %62, 0
// CHECK-NEXT:   %65 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %63, 0
// CHECK-NEXT:   %66 = icmp eq i64 %64, %65
// CHECK-NEXT:   %67 = and i1 true, %66
// CHECK-NEXT:   %68 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %62, 1
// CHECK-NEXT:   %69 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %63, 1
// CHECK-NEXT:   %70 = icmp eq i64 %68, %69
// CHECK-NEXT:   %71 = and i1 %67, %70
// CHECK-NEXT:   %72 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %62, 2
// CHECK-NEXT:   %73 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %63, 2
// CHECK-NEXT:   %74 = call i1 @"{{.*}}/runtime/internal/runtime.StringEqual"(%"{{.*}}/runtime/internal/runtime.String" %72, %"{{.*}}/runtime/internal/runtime.String" %73)
// CHECK-NEXT:   %75 = and i1 %71, %74
// CHECK-NEXT:   %76 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %62, 3
// CHECK-NEXT:   %77 = extractvalue %"{{.*}}/cl/_testgo/equal.T" %63, 3
// CHECK-NEXT:   %78 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" %76, %"{{.*}}/runtime/internal/runtime.eface" %77)
// CHECK-NEXT:   %79 = and i1 %75, %78
// CHECK-NEXT:   %80 = xor i1 %79, true
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %80)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.init#4"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 24)
// CHECK-NEXT:   %1 = getelementptr inbounds i64, ptr %0, i64 0
// CHECK-NEXT:   store i64 1, ptr %1, align 8
// CHECK-NEXT:   %2 = getelementptr inbounds i64, ptr %0, i64 1
// CHECK-NEXT:   store i64 2, ptr %2, align 8
// CHECK-NEXT:   %3 = getelementptr inbounds i64, ptr %0, i64 2
// CHECK-NEXT:   store i64 3, ptr %3, align 8
// CHECK-NEXT:   %4 = insertvalue %"{{.*}}/runtime/internal/runtime.Slice" undef, ptr %0, 0
// CHECK-NEXT:   %5 = insertvalue %"{{.*}}/runtime/internal/runtime.Slice" %4, i64 3, 1
// CHECK-NEXT:   %6 = insertvalue %"{{.*}}/runtime/internal/runtime.Slice" %5, i64 3, 2
// CHECK-NEXT:   %7 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 16)
// CHECK-NEXT:   %8 = call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/internal/runtime.NewSlice2"(ptr %7, i64 8, i64 2, i64 0, i64 2, i1 true, i1 true, i1 true)
// CHECK-NEXT:   %9 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 16)
// CHECK-NEXT:   %10 = call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/internal/runtime.NewSlice2"(ptr %9, i64 8, i64 2, i64 0, i64 0, i1 true, i1 true, i1 true)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   %11 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %6, 0
// CHECK-NEXT:   %12 = icmp ne ptr %11, null
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %12)
// CHECK-NEXT:   %13 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %8, 0
// CHECK-NEXT:   %14 = icmp ne ptr %13, null
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %14)
// CHECK-NEXT:   %15 = extractvalue %"{{.*}}/runtime/internal/runtime.Slice" %10, 0
// CHECK-NEXT:   %16 = icmp ne ptr %15, null
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %16)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.init#5"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 8)
// CHECK-NEXT:   store i64 100, ptr %0, align 8
// CHECK-NEXT:   %1 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_int, ptr undef }, ptr %0, 1
// CHECK-NEXT:   %2 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 0)
// CHECK-NEXT:   store {} zeroinitializer, ptr %2, align 1
// CHECK-NEXT:   %3 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_struct$n1H8J_3prDN3firMwPxBLVTkE5hJ9Di-AqNvaC9jczw", ptr undef }, ptr %2, 1
// CHECK-NEXT:   %4 = alloca %"{{.*}}/cl/_testgo/equal.T", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %4, i8 0, i64 48, i1 false)
// CHECK-NEXT:   %5 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %4, i32 0, i32 0
// CHECK-NEXT:   %6 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %4, i32 0, i32 1
// CHECK-NEXT:   %7 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %4, i32 0, i32 2
// CHECK-NEXT:   %8 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %4, i32 0, i32 3
// CHECK-NEXT:   store i64 10, ptr %5, align 8
// CHECK-NEXT:   store i64 20, ptr %6, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @2, i64 5 }, ptr %7, align 8
// CHECK-NEXT:   %9 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 8)
// CHECK-NEXT:   store i64 1, ptr %9, align 8
// CHECK-NEXT:   %10 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_int, ptr undef }, ptr %9, 1
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.eface" %10, ptr %8, align 8
// CHECK-NEXT:   %11 = load %"{{.*}}/cl/_testgo/equal.T", ptr %4, align 8
// CHECK-NEXT:   %12 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 48)
// CHECK-NEXT:   store %"{{.*}}/cl/_testgo/equal.T" %11, ptr %12, align 8
// CHECK-NEXT:   %13 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_{{.*}}/cl/_testgo/equal.T", ptr undef }, ptr %12, 1
// CHECK-NEXT:   %14 = alloca %"{{.*}}/cl/_testgo/equal.T", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %14, i8 0, i64 48, i1 false)
// CHECK-NEXT:   %15 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %14, i32 0, i32 0
// CHECK-NEXT:   %16 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %14, i32 0, i32 1
// CHECK-NEXT:   %17 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %14, i32 0, i32 2
// CHECK-NEXT:   %18 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %14, i32 0, i32 3
// CHECK-NEXT:   store i64 10, ptr %15, align 8
// CHECK-NEXT:   store i64 20, ptr %16, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @2, i64 5 }, ptr %17, align 8
// CHECK-NEXT:   %19 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 8)
// CHECK-NEXT:   store i64 1, ptr %19, align 8
// CHECK-NEXT:   %20 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_int, ptr undef }, ptr %19, 1
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.eface" %20, ptr %18, align 8
// CHECK-NEXT:   %21 = alloca %"{{.*}}/cl/_testgo/equal.T", align 8
// CHECK-NEXT:   call void @llvm.memset.p0.i64(ptr %21, i8 0, i64 48, i1 false)
// CHECK-NEXT:   %22 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %21, i32 0, i32 0
// CHECK-NEXT:   %23 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %21, i32 0, i32 1
// CHECK-NEXT:   %24 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %21, i32 0, i32 2
// CHECK-NEXT:   %25 = getelementptr inbounds %"{{.*}}/cl/_testgo/equal.T", ptr %21, i32 0, i32 3
// CHECK-NEXT:   store i64 10, ptr %22, align 8
// CHECK-NEXT:   store i64 20, ptr %23, align 8
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @2, i64 5 }, ptr %24, align 8
// CHECK-NEXT:   %26 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 16)
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.String" { ptr @4, i64 2 }, ptr %26, align 8
// CHECK-NEXT:   %27 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_string, ptr undef }, ptr %26, 1
// CHECK-NEXT:   store %"{{.*}}/runtime/internal/runtime.eface" %27, ptr %25, align 8
// CHECK-NEXT:   %28 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 8)
// CHECK-NEXT:   store i64 100, ptr %28, align 8
// CHECK-NEXT:   %29 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @_llgo_int, ptr undef }, ptr %28, 1
// CHECK-NEXT:   %30 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" %1, %"{{.*}}/runtime/internal/runtime.eface" %29)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %30)
// CHECK-NEXT:   %31 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 0)
// CHECK-NEXT:   store {} zeroinitializer, ptr %31, align 1
// CHECK-NEXT:   %32 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_struct$n1H8J_3prDN3firMwPxBLVTkE5hJ9Di-AqNvaC9jczw", ptr undef }, ptr %31, 1
// CHECK-NEXT:   %33 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" %3, %"{{.*}}/runtime/internal/runtime.eface" %32)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %33)
// CHECK-NEXT:   %34 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 0)
// CHECK-NEXT:   store %"{{.*}}/cl/_testgo/equal.N" zeroinitializer, ptr %34, align 1
// CHECK-NEXT:   %35 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_{{.*}}/cl/_testgo/equal.N", ptr undef }, ptr %34, 1
// CHECK-NEXT:   %36 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" %3, %"{{.*}}/runtime/internal/runtime.eface" %35)
// CHECK-NEXT:   %37 = xor i1 %36, true
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %37)
// CHECK-NEXT:   %38 = load %"{{.*}}/cl/_testgo/equal.T", ptr %14, align 8
// CHECK-NEXT:   %39 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 48)
// CHECK-NEXT:   store %"{{.*}}/cl/_testgo/equal.T" %38, ptr %39, align 8
// CHECK-NEXT:   %40 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_{{.*}}/cl/_testgo/equal.T", ptr undef }, ptr %39, 1
// CHECK-NEXT:   %41 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" %13, %"{{.*}}/runtime/internal/runtime.eface" %40)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %41)
// CHECK-NEXT:   %42 = load %"{{.*}}/cl/_testgo/equal.T", ptr %21, align 8
// CHECK-NEXT:   %43 = call ptr @"{{.*}}/runtime/internal/runtime.AllocU"(i64 48)
// CHECK-NEXT:   store %"{{.*}}/cl/_testgo/equal.T" %42, ptr %43, align 8
// CHECK-NEXT:   %44 = insertvalue %"{{.*}}/runtime/internal/runtime.eface" { ptr @"_llgo_{{.*}}/cl/_testgo/equal.T", ptr undef }, ptr %43, 1
// CHECK-NEXT:   %45 = call i1 @"{{.*}}/runtime/internal/runtime.EfaceEqual"(%"{{.*}}/runtime/internal/runtime.eface" %13, %"{{.*}}/runtime/internal/runtime.eface" %44)
// CHECK-NEXT:   %46 = xor i1 %45, true
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %46)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.init#6"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.NewChan"(i64 8, i64 0)
// CHECK-NEXT:   %1 = call ptr @"{{.*}}/runtime/internal/runtime.NewChan"(i64 8, i64 0)
// CHECK-NEXT:   %2 = icmp eq ptr %0, %0
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %2)
// CHECK-NEXT:   %3 = icmp ne ptr %0, %1
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %3)
// CHECK-NEXT:   %4 = icmp ne ptr %0, null
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %4)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.init#7"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.MakeMap"(ptr @"map[_llgo_int]_llgo_string", i64 0)
// CHECK-NEXT:   %1 = icmp ne ptr %0, null
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 %1)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/equal.assert"(i1 true)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/equal.test"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.memequal64"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.memequal64"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.memequal0"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.memequal0"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.nilinterequal"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.nilinterequal"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }

// CHECK-LABEL: define linkonce i1 @"__llgo_stub.{{.*}}/runtime/internal/runtime.memequal8"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = tail call i1 @"{{.*}}/runtime/internal/runtime.memequal8"(ptr %1, ptr %2)
// CHECK-NEXT:   ret i1 %3
// CHECK-NEXT: }
