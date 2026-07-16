//go:build go1.26

package testing_test

import "testing"

func TestGo126Symbols(t *testing.T) {
	var benchmark testing.B
	var fuzz testing.F
	var test testing.T
	_ = benchmark.ArtifactDir
	_ = benchmark.Attr
	_ = benchmark.Output
	_ = fuzz.ArtifactDir
	_ = fuzz.Attr
	_ = fuzz.Output
	_ = test.ArtifactDir
	_ = test.Attr
	_ = test.Output
}
