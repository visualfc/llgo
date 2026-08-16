// LITTEST: POST-ABI darwin/arm64 linux/amd64
// CHECK: target triple = "
// DARWIN-ARM64-SAME: {{.*}}-apple-{{.*}}"
// LINUX-AMD64-SAME: {{.*}}-linux{{.*}}"
package main

type result struct {
	a, b, c int
}

// CHECK-LABEL: define void @main.values(
// CHECK-SAME: ptr sret(%main.result)
func values() result {
	return result{1, 2, 3}
}

func main() {
	_ = values()
}
