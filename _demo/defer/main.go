package main

func main() {
	var a int = 5
	defer println(a)
	defer func() {
		println(a)
	}()
	defer func() {
		println(a + 100)
	}()
	a = 10
	panic("error")
	//Output:
	// 110
	// 10
	// 5
	// panic: error
}
