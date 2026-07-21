package main

//go:noinline
func helper(value NamedInt) NamedInt {
	intermediate := value + 1 // DWARF_LINE_MARKER
	return intermediate
}
