// LITTEST
package main

// CHECK-LABEL: define void @main.main(){{.*}} {
func main() {
	// CHECK: GetThreadDefer
	for i := 0; i < 3; i++ {
		// CHECK: switch i{{(32|64)}}
		defer println("loop", i)
	}
	// CHECK: switch i{{(32|64)}}
	// CHECK: FreeDeferNode
	// CHECK: PrintString
	// CHECK: PrintInt
}
