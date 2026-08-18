//go:build linux || darwin

package gotest

import "testing"

func checkRecoveredFaultAddress(t *testing.T, err error, address *byte) {}
