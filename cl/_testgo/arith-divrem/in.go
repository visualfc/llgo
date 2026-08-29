// LITTEST
package main

// Integer division must guard LLVM's undefined zero-divisor case and the
// signed minInt/-1 overflow while preserving Go's defined result.

// CHECK-LABEL: define i64 @main.signedDiv(
// CHECK: call void @"{{.*}}/runtime/internal/runtime.AssertDivideByZero"
// CHECK: select i1 {{%[0-9]+}}, i64 1, i64 {{%[0-9]+}}
// CHECK: sdiv i64
// CHECK: select i1 {{%[0-9]+}}, i64 {{%[0-9]+}}, i64 {{%[0-9]+}}
func signedDiv(x, y int64) int64 { return x / y }

// CHECK-LABEL: define i64 @main.signedRem(
// CHECK: call void @"{{.*}}/runtime/internal/runtime.AssertDivideByZero"
// CHECK: srem i64
// CHECK: select i1 {{%[0-9]+}}, i64 0, i64 {{%[0-9]+}}
func signedRem(x, y int64) int64 { return x % y }

// CHECK-LABEL: define i64 @main.unsignedDiv(
// CHECK: call void @"{{.*}}/runtime/internal/runtime.AssertDivideByZero"
// CHECK: select i1 {{%[0-9]+}}, i64 1, i64 {{%[0-9]+}}
// CHECK: udiv i64
func unsignedDiv(x, y uint64) uint64 { return x / y }

// CHECK-LABEL: define i64 @main.unsignedRem(
// CHECK: call void @"{{.*}}/runtime/internal/runtime.AssertDivideByZero"
// CHECK: urem i64
func unsignedRem(x, y uint64) uint64 { return x % y }

func main() {
	println(signedDiv(17, 5), signedRem(-17, 5))
	println(unsignedDiv(17, 5), unsignedRem(17, 5))
	min := int64(-1 << 63)
	println(signedDiv(min, -1), signedRem(min, -1))
}
