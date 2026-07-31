// LITTEST
package main

// CHECK: define void @main.main(){{.*}} {
func main() {
	defer func() {
		e := recover()
		println(e.(string))
	}()
	// CHECK: call void @"{{.*}}/runtime/internal/runtime.Panic"
	defer panic("panic in defer")
	println("run main")
}
