//go:build windows

package ffi

// Windows libffi deliberately omits C complex types. Go complex values have
// the same memory and MSVC ABI classification as a two-element homogeneous
// aggregate, so describe them using the equivalent public libffi structure.
func newComplexType(elem *Type, _ uintptr, _ uint16) *Type {
	return StructOf(elem, elem)
}
