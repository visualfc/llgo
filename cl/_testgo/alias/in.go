// LITTEST
package main

type Point struct {
	x float64
	y float64
}

type MyPoint = Point

func (p *MyPoint) Move(dx, dy float64) {
	p.x += dx
	p.y += dy
}

func (p *Point) Scale(factor float64) {
	p.x *= factor
	p.y *= factor
}

func main() {
	pt := &MyPoint{1, 2}
	pt.Scale(2)
	pt.Move(3, 4)
	println(pt.x, pt.y)
}

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/alias.(*Point).Move"(ptr %0, double %1, double %2){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %3 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 0
// CHECK-NEXT:   %4 = load double, ptr %3, align 8
// CHECK-NEXT:   %5 = fadd double %4, %1
// CHECK-NEXT:   %6 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 0
// CHECK-NEXT:   store double %5, ptr %6, align 8
// CHECK-NEXT:   %7 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 1
// CHECK-NEXT:   %8 = load double, ptr %7, align 8
// CHECK-NEXT:   %9 = fadd double %8, %2
// CHECK-NEXT:   %10 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 1
// CHECK-NEXT:   store double %9, ptr %10, align 8
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/alias.(*Point).Scale"(ptr %0, double %1){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %2 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 0
// CHECK-NEXT:   %3 = load double, ptr %2, align 8
// CHECK-NEXT:   %4 = fmul double %3, %1
// CHECK-NEXT:   %5 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 0
// CHECK-NEXT:   store double %4, ptr %5, align 8
// CHECK-NEXT:   %6 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 1
// CHECK-NEXT:   %7 = load double, ptr %6, align 8
// CHECK-NEXT:   %8 = fmul double %7, %1
// CHECK-NEXT:   %9 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 1
// CHECK-NEXT:   store double %8, ptr %9, align 8
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/alias.init"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = load i1, ptr @"{{.*}}/cl/_testgo/alias.init$guard", align 1
// CHECK-NEXT:   br i1 %0, label %_llgo_2, label %_llgo_1
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_1:                                          ; preds = %_llgo_0
// CHECK-NEXT:   store i1 true, ptr @"{{.*}}/cl/_testgo/alias.init$guard", align 1
// CHECK-NEXT:   br label %_llgo_2
// CHECK-EMPTY:
// CHECK-NEXT: _llgo_2:                                          ; preds = %_llgo_1, %_llgo_0
// CHECK-NEXT:   ret void
// CHECK-NEXT: }

// CHECK-LABEL: define void @"{{.*}}/cl/_testgo/alias.main"(){{.*}} {
// CHECK-NEXT: _llgo_0:
// CHECK-NEXT:   %0 = call ptr @"{{.*}}/runtime/internal/runtime.AllocZ"(i64 16)
// CHECK-NEXT:   %1 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 0
// CHECK-NEXT:   %2 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 1
// CHECK-NEXT:   store double 1.000000e+00, ptr %1, align 8
// CHECK-NEXT:   store double 2.000000e+00, ptr %2, align 8
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/alias.(*Point).Scale"(ptr %0, double 2.000000e+00)
// CHECK-NEXT:   call void @"{{.*}}/cl/_testgo/alias.(*Point).Move"(ptr %0, double 3.000000e+00, double 4.000000e+00)
// CHECK-NEXT:   %3 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 0
// CHECK-NEXT:   %4 = load double, ptr %3, align 8
// CHECK-NEXT:   %5 = getelementptr inbounds %"{{.*}}/cl/_testgo/alias.Point", ptr %0, i32 0, i32 1
// CHECK-NEXT:   %6 = load double, ptr %5, align 8
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintFloat"(double %4)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 32)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintFloat"(double %6)
// CHECK-NEXT:   call void @"{{.*}}/runtime/internal/runtime.PrintByte"(i8 10)
// CHECK-NEXT:   ret void
// CHECK-NEXT: }
