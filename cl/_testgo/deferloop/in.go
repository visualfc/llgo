// LITTEST
package main

// CHECK-LABEL: define void @main.main(){{.*}} {
func main() {
	// CHECK: GetThreadDefer
	for i := 0; i < 3; i++ {
		// CHECK: blockaddress(@main.main, %_llgo_6)
		defer println("loop", i)
	}
	// CHECK: PrintString
	// CHECK: PrintInt
	// CHECK: FreeDeferNode
}
