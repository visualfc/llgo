//go:build swissmap

package runtime

import (
	"unsafe"

	"github.com/goplus/llgo/runtime/abi"
	"github.com/goplus/llgo/runtime/internal/runtime/maps"
)

type Map = maps.Map
type maptype = abi.MapType
type arraytype = abi.ArrayType
type structtype = abi.StructType

type slice struct {
	array unsafe.Pointer
	len   int
	cap   int
}

func typedmemmove(typ *_type, dst, src unsafe.Pointer) { Typedmemmove(typ, dst, src) }

func MakeSmallMap() *Map                                               { return makemap_small() }
func MakeMap(t *maptype, hint int) *Map                                { return makemap(t, hint, nil) }
func MapAssign(t *maptype, h *Map, key unsafe.Pointer) unsafe.Pointer  { return mapassign(t, h, key) }
func MapAccess1(t *maptype, h *Map, key unsafe.Pointer) unsafe.Pointer { return mapaccess1(t, h, key) }
func MapAccess2(t *maptype, h *Map, key unsafe.Pointer) (unsafe.Pointer, bool) {
	return mapaccess2(t, h, key)
}
func MapDelete(t *maptype, h *Map, key unsafe.Pointer) { mapdelete(t, h, key) }
func MapClear(t *maptype, h *Map)                      { mapclear(t, h) }

type llgoMapIter struct {
	maps.Iter
	ready bool
}

func NewMapIter(t *maptype, h *Map) *llgoMapIter {
	it := &llgoMapIter{ready: true}
	mapIterStart(t, h, &it.Iter)
	return it
}

func MapIterNext(it *llgoMapIter) (ok bool, k unsafe.Pointer, v unsafe.Pointer) {
	if !it.ready {
		mapIterNext(&it.Iter)
		it.ready = true
	}
	k, v = it.Key(), it.Elem()
	if k == nil {
		return false, nil, nil
	}
	it.ready = false
	return true, k, v
}

func MapLen(h *Map) int {
	if h == nil {
		return 0
	}
	return int(h.Used())
}
