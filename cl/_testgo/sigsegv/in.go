// LITTEST
package main

type T struct {
	s int
}

func f() *T {
	return nil
}

// CHECK: ; Function Attrs: null_pointer_is_valid
// CHECK: define void @"main.init#1"() #0 {
func init() {
	println("init")
	defer func() {
		r := recover()
		if e, ok := r.(error); ok {
			println("recover", e.Error())
		}
	}()
	// CHECK: call ptr @main.f()
	println(f().s)
}

func main() {
	println("main")
}

// CHECK: attributes #0 = { null_pointer_is_valid "frame-pointer"="non-leaf" }
