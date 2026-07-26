// LITTEST
package main

import "reflect"

// SYMBOL-DAG: testdrop/reflect_field_addr_iface{{.*}}RawMessage{{.*}}UnmarshalJSON
// SYMBOL-NOT: testdrop/reflect_field_addr_iface{{.*}}RawMessage{{.*}}drop

type Unmarshaler interface {
	UnmarshalJSON([]byte) error
}

type RawMessage []byte

//go:noinline
func (*RawMessage) UnmarshalJSON([]byte) error {
	return nil
}

//go:noinline
func (*RawMessage) drop() error {
	panic("RawMessage.drop should be unreachable")
}

type Container struct {
	Raw RawMessage
}

func unmarshal(v any) error {
	field := reflect.ValueOf(v).Elem().Field(0)
	return field.Addr().Interface().(Unmarshaler).UnmarshalJSON(nil)
}

func main() {
	if err := unmarshal(&Container{}); err != nil {
		panic(err)
	}
	println(42)
}
