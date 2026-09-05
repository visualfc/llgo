package wasmtest

import (
	"errors"
	"fmt"
	"testing"
)

type wasmError struct{}

func (*wasmError) Error() string { return "wasm error" }
func (*wasmError) Code() int     { return 7 }

type codedError interface {
	error
	Code() int
}

func TestErrorsAsTargets(t *testing.T) {
	// errors.As calls reflectlite.Type.Kind through an interface. Its return
	// type must agree with Go's uint8 Kind, including on Memory64 targets.
	want := &wasmError{}
	err := fmt.Errorf("wrapped: %w", want)
	var ptr *wasmError
	if !errors.As(err, &ptr) || ptr != want {
		t.Fatalf("errors.As pointer target = %v, want %v", ptr, want)
	}
	var iface codedError
	if !errors.As(err, &iface) || iface != want || iface.Code() != 7 {
		t.Fatalf("errors.As interface target = %v, want %v", iface, want)
	}

	// Invalid targets must produce the ordinary recoverable Go panic, not a
	// host trap from a mismatched indirect-call signature.
	var notError int
	for _, target := range []any{42, (*wasmError)(nil), &notError} {
		func() {
			defer func() {
				if recover() == nil {
					t.Errorf("errors.As accepted invalid target %T", target)
				}
			}()
			errors.As(err, target)
		}()
	}
}
