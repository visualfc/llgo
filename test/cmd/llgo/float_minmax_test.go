//go:build go1.21

package llgocmd

import (
	"os"
	"path/filepath"
	"testing"
)

// Keep the reference independent of min/max and ignore unspecified NaN payloads.
const floatMinMaxProbe = `package main

import (
	"fmt"
	"math"
)

type F32 float32
type F64 float64

//go:noinline
func low[T ~float32 | ~float64](a, b T) T { return min(a, b) }

//go:noinline
func high[T ~float32 | ~float64](a, b T) T { return max(a, b) }

//go:noinline
func low3[T ~float32 | ~float64](a, b, c T) T { return min(a, b, c) }

//go:noinline
func high3[T ~float32 | ~float64](a, b, c T) T { return max(a, b, c) }

//go:noinline
func single[T ~float32 | ~float64](a T) T { return min(a) }
func reference[T ~float32 | ~float64](a, b T, upper bool) T {
	if a != a {
		return a
	}
	if b != b {
		return b
	}
	if a == 0 && b == 0 {
		if upper {
			if !math.Signbit(float64(a)) {
				return a
			}
			return b
		}
		if math.Signbit(float64(a)) {
			return a
		}
		return b
	}
	if upper {
		if a > b {
			return a
		}
		return b
	}
	if a < b {
		return a
	}
	return b
}

var count int

func check[T ~float32 | ~float64](got, want T) {
	count++
	if want != want {
		if got != got {
			return
		}
	} else if math.Float64bits(float64(got)) == math.Float64bits(float64(want)) {
		return
	}
	panic(fmt.Sprintf("%T min/max: got %x want %x", got, math.Float64bits(float64(got)), math.Float64bits(float64(want))))
}
func test[T ~float32 | ~float64](values []T) {
	for _, a := range values {
		check(single(a), a)
		for _, b := range values {
			check(low(a, b), reference(a, b, false))
			check(high(a, b), reference(a, b, true))
			for _, c := range values {
				check(low3(a, b, c), reference(reference(a, b, false), c, false))
				check(high3(a, b, c), reference(reference(a, b, true), c, true))
			}
		}
	}
}

var sequence int

func next(n int, v float64) float64 {
	if sequence != n {
		panic("argument evaluation order")
	}
	sequence++
	return v
}
func main() {
	var a []float32
	var b []float64
	var an []F32
	var bn []F64
	for _, u := range []uint32{0, 0x80000000, 0x7fc00001, 0xffc00042, 0x7f800001, 0x7f800000, 0xff800000, 1, 0x80000001, 0x00800000, 0x80800000, 0x3f800000, 0xbf800000, 0x7f7fffff, 0xff7fffff} {
		v := math.Float32frombits(u)
		a = append(a, v)
		an = append(an, F32(v))
	}
	for _, u := range []uint64{0, 0x8000000000000000, 0x7ff8000000000001, 0xfff8000000000042, 0x7ff0000000000001, 0x7ff0000000000000, 0xfff0000000000000, 1, 0x8000000000000001, 0x0010000000000000, 0x8010000000000000, 0x3ff0000000000000, 0xbff0000000000000, 0x7fefffffffffffff, 0xffefffffffffffff} {
		v := math.Float64frombits(u)
		b = append(b, v)
		bn = append(bn, F64(v))
	}
	test(a)
	test(b)
	test(an)
	test(bn)
	check(min(next(0, 1), next(1, math.NaN()), next(2, 2)), math.NaN())
	check(max(next(3, 1), next(4, math.NaN()), next(5, 2)), math.NaN())
	if sequence != 6 {
		panic("argument evaluation count")
	}
	x, y := -3, 4
	s, t := "alpha", "beta"
	if min(x, y) != x || max(x, y) != y || min(s, t) != s || max(s, t) != t {
		panic("non-float min/max")
	}
	fmt.Println("PASS", count)
}
`

func TestFloatMinMaxSemantics(t *testing.T) {
	dir := t.TempDir()
	file := filepath.Join(dir, "main.go")
	if err := os.WriteFile(file, []byte(floatMinMaxProbe), 0644); err != nil {
		t.Fatal(err)
	}
	want, err := runGoCompiler(t, dir, "run", file)
	if err != nil {
		t.Fatalf("gc min/max probe: %v\n%s", err, want)
	}
	modes := []struct {
		name  string
		flags []string
	}{{name: "default"}}
	if toolCompilerName == "llgo" {
		modes = append(modes, struct {
			name  string
			flags []string
		}{"O2", []string{"-O2"}})
	}
	for _, mode := range modes {
		t.Run(mode.name, func(t *testing.T) {
			args := append([]string{"run"}, mode.flags...)
			got, err := runCompiler(t, dir, append(args, file)...)
			if err != nil {
				t.Fatalf("%s min/max probe: %v\n%s", toolCompilerName, err, got)
			}
			if got != want {
				t.Fatalf("%s min/max differs from gc: got %q want %q", toolCompilerName, got, want)
			}
		})
	}
}
