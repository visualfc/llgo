//go:build go1.26

package errors_test

import (
	"errors"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	_ = errors.AsType[error]
}
