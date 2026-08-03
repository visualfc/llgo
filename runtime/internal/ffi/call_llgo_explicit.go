//go:build !llgo || llgo_closure_env_explicit || (llgo && !llgo_closure_env_nest && !llgo_closure_env_swiftself)

package ffi

import "unsafe"

// ClosureEnvExplicit is a compile-time target property.
const ClosureEnvExplicit = true

// CallWithEnv calls fn through ordinary libffi on an explicit-context target.
// When env is required, the caller has already prepended its type and value to
// cif and args; the separately supplied env is therefore intentionally unused.
func CallWithEnv(cif *Signature, fn, _ unsafe.Pointer, ret unsafe.Pointer, args ...unsafe.Pointer) {
	Call(cif, fn, ret, args...)
}
