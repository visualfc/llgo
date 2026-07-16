//go:build go1.26

package trace_test

import (
	"runtime/trace"
	"testing"
)

func TestGo126Symbols(t *testing.T) {
	var recorder trace.FlightRecorder
	var _ trace.FlightRecorderConfig
	_ = recorder.Enabled
	_ = recorder.Start
	_ = recorder.Stop
	_ = recorder.WriteTo
}
