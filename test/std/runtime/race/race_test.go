package race_test

import (
	_ "runtime/race"
	"testing"
)

func TestPackageImports(t *testing.T) {
	t.Log("runtime/race intentionally has no public API")
}
