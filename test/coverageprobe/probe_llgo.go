//go:build llgo

package coverageprobe

import "github.com/xgo-dev/llgo/test/coverageprobe/testdata/dependency"

// CoveredByLLGo is called only by an LLGo-tagged test. Its profile record
// proves that the LLGo test binary, rather than host go test, produced the
// coverage uploaded by CI.
func CoveredByLLGo() int {
	return dependency.CoveredByLLGo()
}

// NotCoveredByLLGo gives the coverage report a stable missed statement too.
func NotCoveredByLLGo() int {
	return dependency.NotCoveredByLLGo()
}
