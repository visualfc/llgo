package main

import (
	"reflect"
	"unsafe"
)

type namedPointer *int
type pointerPoint struct{ X, Y int }
type localUnsafePointer unsafe.Pointer

func (*pointerPoint) Set(x, y int) {}

type metadataMethod interface{ Demo() }

type metadataRType struct{ Flag int }
type metadataUncommon struct{ Offset int }
type metadataEntry struct{ Name string }

func testReflectPointerMetadata() {
	base := reflect.TypeOf((*int)(nil))
	if reflect.PointerTo(base) != reflect.TypeOf((**int)(nil)) {
		panic("PointerTo")
	}
	type pointerField struct{ Value *int }
	if got := reflect.ValueOf(&pointerField{}).Elem().Field(0).Addr().Type(); got != reflect.TypeOf((**int)(nil)) {
		panic("Addr on pointer field")
	}
	named := reflect.TypeOf(namedPointer(nil))
	if named.String()[0] == '*' || reflect.PointerTo(named).String() != "*"+named.String() || reflect.PointerTo(reflect.PointerTo(named)).String() != "**"+named.String() {
		panic("named pointer")
	}
	dynamic := reflect.SliceOf(reflect.TypeOf(pointerPoint{}))
	dynamicPointer := dynamic
	prefix := ""
	for level := 1; level <= 4; level++ {
		dynamicPointer = reflect.PointerTo(dynamicPointer)
		prefix += "*"
		if dynamicPointer.String() != prefix+dynamic.String() {
			panic("dynamic multilevel pointer")
		}
	}

	unsafePointer := reflect.TypeOf(unsafe.Pointer(nil))
	if unsafePointer.Name() != "Pointer" || unsafePointer.PkgPath() != "unsafe" {
		panic("unsafe pointer metadata")
	}
	localUnsafe := reflect.TypeOf(localUnsafePointer(nil))
	if localUnsafe.Name() != "localUnsafePointer" || localUnsafe.PkgPath() == "" || localUnsafe.PkgPath() == "unsafe" {
		panic("local named unsafe pointer metadata")
	}
	point := reflect.TypeOf(pointerPoint{})
	if point.NumMethod() != 0 || reflect.PointerTo(point).NumMethod() != 1 {
		panic("pointer method metadata")
	}
	if reflect.TypeOf((*metadataMethod)(nil)).Elem().PkgPath() == "" {
		panic("interface package metadata")
	}

	// Preserve the dynamic uncommon/method array-size path.
	container := reflect.New(reflect.StructOf([]reflect.StructField{
		{Name: "R", Type: reflect.TypeOf(metadataRType{})},
		{Name: "U", Type: reflect.TypeOf(metadataUncommon{})},
		{Name: "M", Type: reflect.ArrayOf(2, reflect.TypeOf(metadataEntry{}))},
	}))
	if _, ok := container.Elem().Field(2).Slice(0, 2).Interface().([]metadataEntry); !ok {
		panic("dynamic metadata array")
	}
}
