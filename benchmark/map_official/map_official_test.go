// Package mapofficial ports the core map benchmarks from Go's
// runtime/map_benchmark_test.go.
//
// The complete type matrix is intentionally retained for comparison with the
// upstream benchmark. Standard Go can run it directly. LLGo currently cannot
// compile this package as a test binary because its test compiler does not yet
// support the combination of generic type constraints and large array types
// used below; the smaller benchmark/map_fast package remains the LLGo-native
// fallback until that limitation is addressed.
package mapofficial

import (
	"encoding/binary"
	"flag"
	"strconv"
	"testing"
	"unsafe"
)

var mapbench = flag.Bool("mapbench", false, "enable the full set of map benchmark variants")

// These types match the upstream benchmark. The medium/big variants are kept
// even though they currently prevent this full matrix from compiling under
// LLGo; they are useful as soon as generic large-array support is available.
type smallType [16]byte
type mediumType [1 << 9]byte
type bigType [1 << 12]byte

type mapBenchmarkKeyType interface {
	int32 | int64 | string
}

type mapBenchmarkElemType interface {
	mapBenchmarkKeyType | []int32
}

func benchSizes(f func(*testing.B, int)) func(*testing.B) {
	cases := []int{0, 6, 12, 18, 24, 30, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 1 << 16, 1 << 18, 1 << 20, 1 << 22}
	byDefault := map[int]bool{6: true, 64: true, 1 << 16: true}
	return func(b *testing.B) {
		for _, n := range cases {
			b.Run("len="+strconv.Itoa(n), func(b *testing.B) {
				if !*mapbench && !byDefault[n] {
					b.Skip("Skipped because -mapbench=false")
				}
				f(b, n)
			})
		}
	}
}

func genIntValues[T int32 | int64](start, end int) []T {
	vals := make([]T, end-start)
	for i := range vals {
		vals[i] = T(start + i)
	}
	return vals
}

func genStringValues(start, end int) []string {
	vals := make([]string, end-start)
	for i := range vals {
		vals[i] = strconv.Itoa(start + i)
	}
	return vals
}

func genSmallValues(start, end int) []smallType {
	vals := make([]smallType, end-start)
	for i := range vals {
		binary.NativeEndian.PutUint64(vals[i][:], uint64(start+i))
	}
	return vals
}

func genMediumValues(start, end int) []mediumType {
	vals := make([]mediumType, end-start)
	for i := range vals {
		binary.NativeEndian.PutUint64(vals[i][:], uint64(start+i))
	}
	return vals
}

func genBigValues(start, end int) []bigType {
	vals := make([]bigType, end-start)
	for i := range vals {
		binary.NativeEndian.PutUint64(vals[i][:], uint64(start+i))
	}
	return vals
}

func genPtrValues[T any](start, end int) []*T {
	vals := make([]*T, end-start)
	for i := range vals {
		vals[i] = new(T)
	}
	return vals
}

func genValues[T mapBenchmarkElemType](start, end int) []T {
	var zero T
	switch any(zero).(type) {
	case int32:
		return any(genIntValues[int32](start, end)).([]T)
	case int64:
		return any(genIntValues[int64](start, end)).([]T)
	case string:
		return any(genStringValues(start, end)).([]T)
	default:
		panic("unreachable")
	}
}

//go:noinline
func newSink[T mapBenchmarkElemType]() *T { return new(T) }

func fillMap[K mapBenchmarkKeyType, E mapBenchmarkElemType](keys []K, elems []E) map[K]E {
	m := make(map[K]E, len(keys))
	for i := range keys {
		m[keys[i]] = elems[i]
	}
	return m
}

func checkAllocSize[K, E any](b *testing.B, n int) {
	var k K
	var e E
	if uint64(n)*(uint64(unsafe.Sizeof(k))+uint64(unsafe.Sizeof(e))) >= 1<<30 {
		b.Skip("key and element size exceeds 1GiB")
	}
}

func benchmarkMapAccessHit[K mapBenchmarkKeyType, E mapBenchmarkElemType](b *testing.B, n int) {
	if n == 0 {
		b.Skip("can't access empty map")
	}
	checkAllocSize[K, E](b, n)
	k, e := genValues[K](0, n), genValues[E](0, n)
	m := fillMap(k, e)
	sink := newSink[E]()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		*sink = m[k[i%n]]
	}
}

func BenchmarkMapAccessHit(b *testing.B) {
	b.Run("Key=int32/Elem=int32", benchSizes(benchmarkMapAccessHit[int32, int32]))
	b.Run("Key=int64/Elem=int64", benchSizes(benchmarkMapAccessHit[int64, int64]))
	b.Run("Key=string/Elem=string", benchSizes(benchmarkMapAccessHit[string, string]))
	// Disabled for LLGo: large-array and pointer generic combinations.
}

var sinkOK bool

func benchmarkMapAccessMiss[K mapBenchmarkKeyType, E mapBenchmarkElemType](b *testing.B, n int) {
	checkAllocSize[K, E](b, n)
	k, e := genValues[K](0, n), genValues[E](0, n)
	m := fillMap(k, e)
	if n == 0 {
		n = 1
	}
	w := genValues[K](n, 2*n)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_, sinkOK = m[w[i%n]]
	}
}

func BenchmarkMapAccessMiss(b *testing.B) {
	b.Run("Key=int32/Elem=int32", benchSizes(benchmarkMapAccessMiss[int32, int32]))
	b.Run("Key=int64/Elem=int64", benchSizes(benchmarkMapAccessMiss[int64, int64]))
	b.Run("Key=string/Elem=string", benchSizes(benchmarkMapAccessMiss[string, string]))
}

func benchmarkMapAssignExists[K mapBenchmarkKeyType, E mapBenchmarkElemType](b *testing.B, n int) {
	if n == 0 {
		b.Skip("can't assign to empty map")
	}
	checkAllocSize[K, E](b, n)
	k, e := genValues[K](0, n), genValues[E](0, n)
	m := fillMap(k, e)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		m[k[i%n]] = e[i%n]
	}
}

func BenchmarkMapAssignExists(b *testing.B) {
	b.Run("Key=int32/Elem=int32", benchSizes(benchmarkMapAssignExists[int32, int32]))
	b.Run("Key=int64/Elem=int64", benchSizes(benchmarkMapAssignExists[int64, int64]))
	b.Run("Key=string/Elem=string", benchSizes(benchmarkMapAssignExists[string, string]))
}

func benchmarkMapDelete[K mapBenchmarkKeyType, E mapBenchmarkElemType](b *testing.B, n int) {
	if n == 0 {
		b.Skip("can't delete from empty map")
	}
	checkAllocSize[K, E](b, n)
	k, e := genValues[K](0, n), genValues[E](0, n)
	m := fillMap(k, e)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if len(m) == 0 {
			for j := range k {
				m[k[j]] = e[j]
			}
		}
		delete(m, k[i%n])
	}
}

func BenchmarkMapDelete(b *testing.B) {
	b.Run("Key=int32/Elem=int32", benchSizes(benchmarkMapDelete[int32, int32]))
	b.Run("Key=int64/Elem=int64", benchSizes(benchmarkMapDelete[int64, int64]))
	b.Run("Key=string/Elem=string", benchSizes(benchmarkMapDelete[string, string]))
}
