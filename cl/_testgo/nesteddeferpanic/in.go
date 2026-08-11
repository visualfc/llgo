// LITTEST
package main

// CHECK-LABEL: define void @main.replacement(){{.*}} {
func replacement() {
	defer func() {
		println("replacement", recover().(int))
	}()

	func() {
		for {
			defer func() {
				defer panic(5)
			}()
			break
		}
		panic(4)
	}()
}

func resumeOuterPanic() {
	defer func() {
		println("resume outer", recover().(int))
	}()

	func() {
		defer func() {
			defer func() {
				println("resume inner", recover().(int))
			}()
			panic(2)
		}()
		panic(1)
	}()
}

func recoverThenPanic() {
	defer func() {
		println("recover-then-panic outer", recover().(int))
	}()

	func() {
		defer func() {
			old := recover().(int)
			defer panic(3)
			println("recover-then-panic old", old)
		}()
		panic(2)
	}()
}

func main() {
	replacement()
	resumeOuterPanic()
	recoverThenPanic()
}
