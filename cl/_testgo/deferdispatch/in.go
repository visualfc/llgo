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
// CHECK: blockaddress
// CHECK: indirectbr ptr
// CHECK: indirectbr ptr
// CHECK: }
