//go:build go1.26

package errors_test

import (
	"errors"
	"fmt"
	"io"
	"os"
	"testing"
)

func TestAsType(t *testing.T) {
	want := &os.PathError{Op: "open", Path: "missing", Err: os.ErrNotExist}
	got, ok := errors.AsType[*os.PathError](fmt.Errorf("wrapped: %w", want))
	if !ok || got != want {
		t.Fatalf("AsType[*os.PathError] = %#v, %v; want %#v, true", got, ok, want)
	}
	if got, ok := errors.AsType[*os.PathError](io.EOF); ok || got != nil {
		t.Fatalf("AsType on io.EOF = %#v, %v; want nil, false", got, ok)
	}
}
