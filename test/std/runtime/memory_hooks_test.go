package runtime_test

import (
	"bytes"
	"testing"
	"unsafe"
)

//go:linkname runtimeMemmove runtime.memmove
func runtimeMemmove(dst, src unsafe.Pointer, n uintptr)

//go:linkname runtimeMemclr runtime.memclrNoHeapPointers
func runtimeMemclr(ptr unsafe.Pointer, n uintptr)

// Resolve the addresses at initialization, as external runtime consumers do.
var runtimeMove = runtimeMemmove
var runtimeClear = runtimeMemclr

func TestMemoryMoveHook(t *testing.T) {
	direct := []byte{1, 2, 3, 4}
	runtimeMemmove(unsafe.Pointer(&direct[1]), unsafe.Pointer(&direct[0]), 3)
	if !bytes.Equal(direct, []byte{1, 1, 2, 3}) {
		t.Fatalf("direct memmove = %v", direct)
	}
	runtimeMemmove(nil, nil, 0)

	// Exercise direct and indirect calls, unaligned ranges, both overlap
	// directions, identical addresses, and a size larger than a machine word.
	for _, move := range []func(unsafe.Pointer, unsafe.Pointer, uintptr){runtimeMemmove, runtimeMove} {
		move(nil, nil, 0)
		for _, size := range []int{0, 1, 2, 7, 8, 15, 16, 31, 32, 65, 257} {
			for _, offset := range [][2]int{{0, 0}, {1, 0}, {0, 1}, {3, 17}, {17, 3}, {300, 0}} {
				got := make([]byte, 600)
				for i := range got {
					got[i] = byte(i*31 + 7)
				}
				want := append([]byte(nil), got...)
				dst, src := offset[0], offset[1]
				copy(want[dst:dst+size], want[src:src+size])
				move(unsafe.Pointer(&got[dst]), unsafe.Pointer(&got[src]), uintptr(size))
				if !bytes.Equal(got, want) {
					t.Fatalf("memmove dst=%d src=%d size=%d changed the wrong bytes", dst, src, size)
				}
			}
		}
	}
}

func TestMemoryClearHook(t *testing.T) {
	direct := []byte{1, 2, 3, 4}
	runtimeMemclr(unsafe.Pointer(&direct[1]), 2)
	if !bytes.Equal(direct, []byte{1, 0, 0, 4}) {
		t.Fatalf("direct memclr = %v", direct)
	}
	runtimeMemclr(nil, 0)

	for _, clearMemory := range []func(unsafe.Pointer, uintptr){runtimeMemclr, runtimeClear} {
		clearMemory(nil, 0)
		for _, size := range []int{0, 1, 2, 7, 8, 15, 16, 31, 32, 65, 257} {
			for _, offset := range []int{0, 1, 3, 17} {
				got := bytes.Repeat([]byte{0xa5}, 300)
				want := append([]byte(nil), got...)
				for i := offset; i < offset+size; i++ {
					want[i] = 0
				}
				clearMemory(unsafe.Pointer(&got[offset]), uintptr(size))
				if !bytes.Equal(got, want) {
					t.Fatalf("memclr offset=%d size=%d changed the wrong bytes", offset, size)
				}
			}
		}
	}
}
