// LITTEST
package main

// CHECK-LABEL: define i32 @main.f(i32 %0){{.*}} {
// CHECK: add i32 %0, 1
func f(a uint32) uint32 {
	a++
	return a
}

// CHECK-LABEL: define void @main.main(){{.*}} {
// CHECK: zext i32 {{%[0-9]+}} to i64
// CHECK: call void @"{{.*}}PrintUint"
func main() {
	var a uint32 = 100
	println(f(a))
}
