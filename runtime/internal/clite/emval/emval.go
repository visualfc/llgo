package emval

import (
	_ "unsafe"

	c "github.com/goplus/llgo/runtime/internal/clite"
)

const (
	LLGoFiles   = "_wrap/emval.cpp"
	LLGoPackage = "link: -lembind"
)

const (
	UNDEFINED     Value = 2
	NULL          Value = 4
	TRUE          Value = 6
	FALSE         Value = 8
	LAST_RESERVED Value = 8
)

const (
	FUNCTION    = 0
	CONSTRUCTOR = 1
)

type Value uintptr

//go:linkname GetGlobal C.llgo_emval_get_global
func GetGlobal(name *c.Char) Value

//go:linkname NewDouble C.llgo_emval_new_double
func NewDouble(v float64) Value

//go:linkname NewString C.llgo_emval_new_string
func NewString(str *c.Char) Value

//go:linkname NewObject C.llgo_emval_new_object
func NewObject() Value

//go:linkname NewArray C.llgo_emval_new_array
func NewArray() Value

//go:linkname SetProperty C.llgo_emval_set_property
func SetProperty(object Value, key Value, value Value)

//go:linkname GetProperty C.llgo_emval_get_property
func GetProperty(object Value, key Value) Value

//go:linkname Delete C.llgo_emval_delete
func Delete(object Value, property Value) bool

//go:linkname IsNumber C.llgo_emval_is_number
func IsNumber(object Value) bool

//go:linkname IsString C.llgo_emval_is_string
func IsString(object Value) bool

//go:linkname In C.llgo_emval_in
func In(item Value, object Value) bool

//go:linkname Typeof C.llgo_emval_typeof
func Typeof(value Value) Value

//go:linkname Instanceof C.llgo_emval_instanceof
func Instanceof(object Value, constructor Value) bool

//go:linkname AsDouble C.llgo_emval_as_double
func AsDouble(v Value) float64

//go:linkname AsString C.llgo_emval_as_string
func AsString(v Value) string

//go:linkname Equals C.llgo_emval_equals
func Equals(first Value, second Value) bool

//go:linkname MethodCall C.llgo_emval_method_call
func MethodCall(object Value, name *c.Char, args *Value, nargs c.Int, err *c.Int) Value

//go:linkname Call C.llgo_emval_call
func Call(fn Value, args *Value, nargs c.Int, kind c.Int, err *c.Int) Value

//go:linkname SetInvokeCallback C.llgo_emval_set_invoke
func SetInvokeCallback(fn func(args Value) bool)

//export llgo_export_string_from
func llgo_export_string_from(data *c.Char, size c.Int) string {
	return c.GoString(data, size)
}
