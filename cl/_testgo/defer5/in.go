// LITTEST
package main

// CHECK-LABEL: define void @main.main(){{.*}} {
func main() {
	// CHECK: GetThreadDefer
	// CHECK: switch i{{(32|64)}}
	// CHECK: call void @"main.main$1"()
	// CHECK: call void @"main.main$2"()
	// CHECK: FreeDeferNode
	// CHECK: FreeDeferNode
	// CHECK: switch i{{(32|64)}}
	defer println("A")
	defer func() {
		if e := recover(); e != nil {
			println("in defer 1")
			panic("panic in defer 1")
		}
	}()
	defer func() {
		println("in defer 2")
		panic("panic in defer 2")
	}()
	defer println("B")
	panic("panic in main")
}

// CHECK-LABEL: define void @"main.main$1"(){{.*}} {
// CHECK: Recover
// CHECK: PrintString
// CHECK: PrintByte
// CHECK-LABEL: define void @"main.main$2"(){{.*}} {
// CHECK: PrintString
// CHECK: PrintByte
