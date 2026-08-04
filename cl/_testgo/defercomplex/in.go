// LITTEST
package main

// CHECK-DAG: call ptr @"{{.*}}/runtime/internal/runtime.GetThreadDefer"()
// CHECK-DAG: call void @"{{.*}}/runtime/internal/runtime.SetThreadDefer"(ptr %{{.*}})
// CHECK-DAG: define void @"main.complexOrder$1"(ptr {{((nest|swiftself) )?}}%{{.*}}, %"{{.*}}/runtime/internal/runtime.String" %{{.*}}){{.*}} {
// CHECK-DAG: call %"{{.*}}/runtime/internal/runtime.Slice" @"{{.*}}/runtime/internal/runtime.SliceAppend"({{.*}})

func main() {
	for _, label := range complexOrder() {
		println(label)
	}
}

func complexOrder() (res []string) {
	record := func(label string) { res = append(res, label) }

	defer record(label1("cleanup-final", 0))
	defer record(label1("cleanup-before-loop", 0))

	for i := 0; i < 2; i++ {
		defer record(label1("exit-outer", i))
		for j := 0; j < 2; j++ {
			if j == 0 {
				defer record(label2("branch-even", i, j))
			} else {
				defer record(label2("branch-odd", i, j))
			}
			for k := 0; k < 2; k++ {
				nested := label3("nested", i, j, k)
				defer record(nested)
				if k == 1 {
					defer record(label3("nested-tail", i, j, k))
				}
			}
		}
	}

	defer record(label1("post-loop", 0))
	return
}

func label1(prefix string, a int) string {
	return prefix + "-" + digit(a)
}

func label2(prefix string, a, b int) string {
	return prefix + "-" + digit(a) + "-" + digit(b)
}

func label3(prefix string, a, b, c int) string {
	return prefix + "-" + digit(a) + "-" + digit(b) + "-" + digit(c)
}

func digit(n int) string {
	return string(rune('0' + n))
}
