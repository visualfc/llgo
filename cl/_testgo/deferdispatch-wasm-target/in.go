// LITTEST
package main

func run() {
	defer println("first")
	defer println("second")
}

func main() {
	run()
}

// CHECK-LABEL: define void @main.run()
// CHECK-NOT: blockaddress
// CHECK: switch i32
// CHECK-NOT: blockaddress
// CHECK: switch i32
// CHECK-NOT: blockaddress
// CHECK-NOT: indirectbr
// CHECK: }
