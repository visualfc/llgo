//go:build go1.26

package mlkem_test

import (
	"crypto/mlkem"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var key768 mlkem.DecapsulationKey768
	var key1024 mlkem.DecapsulationKey1024
	_ = key768.Encapsulator
	_ = key1024.Encapsulator
}
